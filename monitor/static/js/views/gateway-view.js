define([
    'views/base/view',
    'text!html/gateway.html'
], function(View, template) {
    'use strict';

    var GatewayView = View.extend({
        autoRender: true,
        autoAttach: true,
        noWrap: true,
        template: template,
        getTemplateData: function () {
            return {
                message: 'Some data to bind'
            }
        }
    });

    return GatewayView;
});