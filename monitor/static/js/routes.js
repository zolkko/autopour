(function (factory) {
    'use strict';
    if (typeof define === 'function' && define.amd) {
        return define(factory);
    } else {
        window.Routes = factory();
    }
}(function () {
    'use strict';

    return function(match) {
        match('', 'plants#index');
        match('plants', 'plants#index');
        match('plants/:id', 'plants#details'),
        match('gateway', 'gateway#index');
    };
}));