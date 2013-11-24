define([
    'chaplin'
], function(Chaplin) {
    'use strict';

    var PlantStatusModel = Chaplin.Model.extend({
        test: function () {
            return 'Hello all';
        }
    });

    return PlantStatusModel;
});