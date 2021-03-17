var app = angular.module("Paragraph", []);

app.factory("ParagraphService", function() {
    var parameters = ["help_url", "batch_cmd", "pom_xml"];
    return {
        all: function() { return parameters; },
        first: function() { return parameters[0]; }
    };
});

app.controller("ParagraphCtrl", function($scope, ParagraphService) {
    $scope.parameters = ParagraphService.all();
});
