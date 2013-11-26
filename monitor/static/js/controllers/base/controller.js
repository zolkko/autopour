(function (factory) {
    'use strict';
    if (typeof define === 'function' && define.amd) {
        return define([
            'chaplin'
        ], factory);
    } else {
        window.BaseController = factory(window.Chaplin/*, window.SiteView*/);
    }
}(function (Chaplin) {
    'use strict';

    var Controller = Chaplin.Controller.extend({
        dimmer: null,
        getDimmer: function () {
            if (!this.dimmer) {
                this.dimmer = document.getElementsByClassName('dimmer')[0]
            }
            return this.dimmer;
        },
        disableDimmer: function () {
            this.getDimmer().className = this.getDimmer().className.replace(/\bactive\b/, '');
        },
        enableDimmer: function () {
            if (!this.getDimmer().className.match(/\bactive\b/)) {
                this.getDimmer().className += ' active';
            }
        },
        beforeAction: function (o, route, data) {
            this.setActiveMenu(route);
        },
        getMenuItem: function () {
            return null;
        },
        setActiveMenu: function (route) {
            if (!route || !route.previous || route.controller != route.previous.controller) {
                var i,
                    elements =  document.querySelectorAll('.main-menu a.active'),
                    menuItem = this.getMenuItem();

                for (i = 0; i < elements.length; i++) {
                    elements[i].className = elements[i].className.replace(/\bactive\b/, '');
                }

                if (menuItem) {
                    menuItem.className += ' active';
                }
            }
        }
    });

    return Controller;
}));