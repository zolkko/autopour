(function (factory) {
    'use strict';
    if (typeof define === 'function' && define.amd) {
        return define([
            'chaplin'/*,
            'views/site-view'*/
        ], factory);
    } else {
        window.BaseController = factory(window.Chaplin/*, window.SiteView*/);
    }
}(function (Chaplin/*, SiteView*/) {
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
        beforeAction: function () {
            var i,
                elements =  document.querySelectorAll('.main-menu a.active');
            for (i = 0; i < elements.length; i++) {
                elements[i].className = elements[i].className.replace(/\bactive\b/, '');
            }
        }
    });

    return Controller;
}));