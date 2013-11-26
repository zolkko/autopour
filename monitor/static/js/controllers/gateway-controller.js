define([
    'controllers/base/controller',
    'views/gateway-view'
], function(Controller, ControlView) {
    'use strict';

    var GatewayController = Controller.extend({
        menuItem: null,
        getMenuItem: function () {
            if (!this.menuItem) {
                this.menuItem = document.querySelector('a.item[href="/gateway"]');
            }
            return this.menuItem;
        },
        index: function(params) {
            this.disableDimmer();
            this.view = new ControlView({
                container: document.getElementsByClassName('content')[0]
            });
        }
    });

    return GatewayController;
});