require.config({
    baseUrl: '/static/js',
    /*map: {
        '*': { 'exoskeleton': 'exoskeleton.min' }
    },*/
    paths: {
        text: 'text',
        jquery: 'zepto.min',
		underscore: 'lodash.min',
        backbone: 'exoskeleton.min',
        handlebars: 'handlebars',
        chaplin: 'chaplin'
	},
	shim: {
        // underscore: { exports: '_' },
        // backbone: { exports: 'Backbone', deps: ['underscrore']},
        jquery: { exports: '$' },
        handlebars: {exports: 'Handlebars'}
	}
});

/*
define('jquery', function() {});
define('underscore', ['exoskeleton'], function(Exoskel) {
    return Exoskel.utils;
});
define('backbone', ['exoskeleton'], function (Exoskel) {
    return Exoskel;
});
*/

require(['app', 'routes'], function (App, routes) {
    new App({
        routes: routes,
        controllerSuffix: '-controller'
    });
});