:- module(paragraph_bdsl, [ ':>'/2, '-+'/2, p/1, d/2, z/2, f/1, s/1, i/1 ]).

:- op(500, xfy, ':>').
:- op(400, xfy, '-+').
:- dynamic ':>'/2.
:- discontiguous ':>'/2.
:- dynamic '-+'/2.
:- discontiguous '-+'/2.
:- dynamic p/1.
:- discontiguous p/1.  % path specification
:- dynamic d/2.
:- discontiguous d/2.  % directory specification
:- dynamic z/2.
:- discontiguous z/2.  % zip/jar specification
:- dynamic f/1.
:- discontiguous f/1.  % file specification
:- dynamic s/1.
:- discontiguous s/1.  % structured parameter specification
:- dynamic i/1.
:- discontiguous i/1.  % individual parameter specification 

