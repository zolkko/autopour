require.config({
    baseUrl: '/static/js',
    paths: {
        text: 'text',
        jquery: 'zepto.min',
		underscore: 'lodash.min',
        backbone: 'exoskeleton.min',
        handlebars: 'handlebars',
        chaplin: 'chaplin.min',
        kinetic: 'kinetic-v4.7.4.min'
	},
	shim: {
        jquery: { exports: '$' },
        handlebars: {exports: 'Handlebars'}
	}
});

require(['app', 'routes'], function (App, routes) {
    new App({
        routes: routes,
        controllerSuffix: '-controller'
    });
});