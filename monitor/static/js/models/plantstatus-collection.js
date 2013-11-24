define([
    'chaplin',
    'models/plantstatus'
], function(Chaplin, PlantStatusModel) {
    'use strict';

    var PlantStatusModel = Chaplin.Collection.extend({
        model: PlantStatusModel,
        url: '/api/plants'
    });

    return PlantStatusModel;
});