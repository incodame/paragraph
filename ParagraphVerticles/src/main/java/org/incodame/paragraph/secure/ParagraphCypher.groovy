package org.incodame.paragraph.secure

import org.apache.commons.codec.binary.Base64

import javax.crypto.Cipher
import javax.crypto.SecretKey
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.PBEKeySpec
import javax.crypto.spec.SecretKeySpec
import java.nio.charset.StandardCharsets
import java.security.AlgorithmParameters
import java.security.spec.KeySpec

class ParagraphCipher {

    private Properties cipherProps
    private File cipherPropsFile
    private byte[] salt

    public ParagraphCipher(String propsFile) {
        this.salt = "27059345".getBytes()
        this.cipherPropsFile = new File(propsFile)
        if (cipherPropsFile.exists()) {
            // read file to cipherProps
            Properties properties = new Properties()
            cipherPropsFile.withInputStream {
                properties.load(it)
            }
            this.cipherProps = properties
        } else {
            // create cipherProps
            this.cipherProps = new Properties()
        }
    }

    public SecretKey buildSecretKey(String key) {
        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
        KeySpec spec = new PBEKeySpec(key.getChars(), salt, 65536, 256)
        SecretKey tmp = factory.generateSecret(spec)
        SecretKey aesKey = new SecretKeySpec(tmp.getEncoded(), "AES")
        return aesKey
    }

    public void setProps(String hash, String key, String p) {
        cipherProps.setProperty("hash", hash)
        SecretKey aesKey = buildSecretKey(key)
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
        cipher.init(Cipher.ENCRYPT_MODE, aesKey)
        AlgorithmParameters params = cipher.getParameters()
        byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV()
        byte[] encrypted = cipher.doFinal(p.getBytes(StandardCharsets.US_ASCII))
        String encryptedStr = Base64.encodeBase64String(encrypted)
        cipherProps.setProperty("p", encryptedStr)
        cipherProps.setProperty("iv", new String(iv))
        cipherPropsFile.withWriterAppend("US-ASCII") { fileWriter ->
            fileWriter.writeLine ''
            cipherProps.each { k, v ->
                fileWriter.writeLine "$k=$v"
            }
        }
    }

    /* method called upon login */
    public String hash4Key(String loginKeyStr) {
        SecretKey aesKey = buildSecretKey(loginKeyStr)
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
        byte[] iv = cipherProps.getProperty("iv").getBytes(StandardCharsets.US_ASCII)
        cipher.init(Cipher.DECRYPT_MODE, aesKey, new IvParameterSpec(iv))
        SingleCipher decryptCipher = SingleCipher.instance
        decryptCipher.init(cipher)
        return cipherProps.getProperty("hash")
    }

    public String nodeciph() {
        return cipherProps.getProperty("p")
    }

    public String decipher() {
        SingleCipher decryptCipher = SingleCipher.instance
        return decryptCipher.decipher(cipherProps.getProperty("p"))
    }

    @Singleton
    class SingleCipher {
        Cipher dCipher
        def init(Cipher c) { dCipher = c }
        String decipher(String s) { return new String(dCipher.doFinal(Base64.decodeBase64(s))) }
    }
}
