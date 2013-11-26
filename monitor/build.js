({
	appDir: './static',
	baseUrl: 'js',
	// mainConfigFile: 'static/js/main.js',
    paths: {
        text: 'text',
        jquery: 'zepto.min',
		underscore: 'lodash.min',
        backbone: 'exoskeleton.min',
        handlebars: 'handlebars',
        chaplin: 'chaplin.min'
	},
	shim: {
        jquery: { exports: '$' },
        handlebars: {exports: 'Handlebars'}
	},

    inlineText: true,
    optimize: 'uglify2',
    skipDirOptimize: true,
    generateSourceMaps: false,
    preserveLicenseComments: true,
    skipModuleInsertion: true,
    removeCombined: true,
    modules: [
        {
            name: 'main',
            include: [
                'controllers/plants-controller',
                'controllers/gateway-controller'
            ],
            exclude: [
                'chaplin',
                'handlebars',
                'underscore', 'lodash.min',
                'backbone', 'exoskeleton.min',
                'jquery', 'zepto.min'
            ]
        }
    ],
	dir: 'build'
})