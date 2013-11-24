define([
    'controllers/base/controller',
    'views/plants-view',
    'models/plantstatus',
    'models/plantstatus-collection'
], function(Controller, PlantsView, PlantStatusModel, PlantStatusCollection) {
    'use strict';

    var PlantsController = Controller.extend({
        index: function() {
            document.querySelector('a.item[href="/plants"]').className += ' active';
            this.view = new PlantsView({
                container: document.getElementsByClassName('content')[0],
                collection: new PlantStatusCollection()
            });

            this.listenToOnce(this.view.collection, 'request', (function () {
                this.enableDimmer();
            }).bind(this));

            this.listenToOnce(this.view.collection, 'sync', (function () {
                this.disableDimmer();
            }).bind(this));

            this.view.collection.fetch()
        },

        details: function () {
        }
    });
    return PlantsController;
});