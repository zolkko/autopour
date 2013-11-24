define([
    'views/base/view',
    'text!html/plants-item.html'
], function(View, template) {
    'use strict';

    var StatsItemView = View.extend({
        autoRender: true,
        autoAttach: true,
        noWrap: true,
        template: template,
        getTemplateData: function () {
            var data = this.model.toJSON();
            if (this.model.isNew()) {
                data.detailsUrl = '/plants';
            } else {
                data.detailsUrl = '/plants/' + this.model.id;
            }
            return data;
        }
    });

    return StatsItemView;
});