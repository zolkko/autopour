(function (factory) {
    'use strict';
    if (typeof define === 'function' && define.amd) {
        return define([
            'chaplin'
        ], factory);
    } else {
        window.Application = factory(window.Chaplin);
    }
}(function (Chaplin) {
	'use strict';

    var Application = Chaplin.Application.extend({
        title: 'AutoPour Monitor',
        start: function () {
            Chaplin.Application.prototype.start.apply(this, arguments);
        }
    });

    return Application;
}));