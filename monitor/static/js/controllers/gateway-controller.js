define([
    'controllers/base/controller',
    'views/gateway-view'
], function(Controller, ControlView) {
    'use strict';

    var GatewayController = Controller.extend({
        index: function(params) {
            this.disableDimmer();
            document.querySelector('a.item[href="/gateway"]').className += ' active';

            this.view = new ControlView({
                container: document.getElementsByClassName('content')[0]
            });
        }
    });

    return GatewayController;
});