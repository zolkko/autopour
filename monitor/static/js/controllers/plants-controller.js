define([
    'controllers/base/controller',
    'views/plants-view',
    'views/plant-details-view',
    'models/plantstatus',
    'models/plantstatus-collection'
], function(Controller, PlantsView, PlantDetailsView, PlantStatusModel, PlantStatusCollection) {
    'use strict';

    var PlantsController = Controller.extend({
        menuItem: null,
        getMenuItem: function () {
            if (!this.menuItem) {
                this.menuItem = document.querySelector('a.item[href="/plants"]');
            }
            return this.menuItem;
        },
        index: function() {
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
        details: function (plantId) {
            this.disableDimmer();
            this.view = new PlantDetailsView({
                container: document.getElementsByClassName('content')[0]
            });
        }
    });
    return PlantsController;
});