define([
    'views/base/collectionview',
    'views/plants-item-view',
    'text!html/plants.html'
], function(CollectionView, ItemView, template) {
    'use strict';

    var StatsView = CollectionView.extend({
        autoRender: true,
        autoAttach: true,
        noWrap: true,
        renderItems: true,
        itemView: ItemView,
        template: template,
        listSelector: 'div.list',
        loadingSelector: 'div.loader',
    });

    return StatsView;
});