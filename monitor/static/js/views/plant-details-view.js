define([
    'views/base/view',
    'kinetic',
    'text!html/plants-details.html'
], function(View, Kinetic, template) {
    'use strict';

    var PlantDetailsView = View.extend({
        autoRender: true,
        autoAttach: true,
        noWrap: true,
        template: template,
        getTemplateData: function () {
            return {
            };
        },
        render: function () {
            View.prototype.render.apply(this, arguments);
            this.drawChart();
        },
        drawChart: function () {
            var chartContainer = this.el.querySelector('.plant-chart');
            var stage = new Kinetic.Stage({
                container: chartContainer,
                width: 800,
                height: 400
            });
            var layer = new Kinetic.Layer();
            var rect = new Kinetic.Rect({
                x: 0,
                y: 0,
                width: 800,
                height: 400,
                fill: 'white',
                stroke: '#c0c0c0',
                strokeWidth: 2
            });
            layer.add(rect);

            for (var i = 10; i < 800; i += 40) {
                var line = new Kinetic.Line({
                    points: [i, 10, i, 390],
                    stroke: '#707070',
                    strokeWidth: 0.5,
                    lineJoin: 'round',
                    dashArray: [2, 4]
                });
                layer.add(line);
            }

            for (var i = 10; i < 400; i += 40) {
                var line = new Kinetic.Line({
                    points: [10, i, 790, i],
                    stroke: '#707070',
                    strokeWidth: 0.5,
                    lineJoin: 'round',
                    dashArray: [2, 4]
                });
                layer.add(line);
            }

            var points = new Array();
            for (var i = 20; i < 790; i += 20) {
                points.push(i);
                points.push((Math.random() * 1000) % 360 + 20);
            }

            var line = new Kinetic.Spline({
                points: points,
                tension: 0.3,
                stroke: '#1AB8F3',
                strokeWidth: 2,
                dashArrayEnabled: false,
                // shadowEnabled: false,
                shadowColor: 'black',
                shadowOffsetX: 0,
                shadowOffsetY: 0,
                shadowBlur: 0,
                fillEnabled: false,
                lineJoin: 'round',
                offsetX: -5.5,
                offsetY: 0
            });
            layer.add(line);

            var circs = new Array();
            for (var x = 0; x < points.length; x += 2) {
                var c = new Kinetic.Circle({
                    radius: 4,
                    fill: 'white',
                    stroke: '#1AB8F3',
                    strokeWidth: 2,
                    x: points[x],
                    y: points[x + 1],
                    offsetX: -5.5,
                    offsetY: 0
                });
                circs.push(c);
                layer.add(c);
            }

            /* vertical line */
            layer.add(new Kinetic.Line({
                points: [15, 20, 20, 20, 20, 380, 15, 380],
                stroke: 'black',
                strokeWidth: 1,
                dashArrayEnabled: false,
                shadowEnabled: false,
                fillEnabled: false,
                offsetX: 0.5,
                offsetY: 0.5
            }));

            /* text markers */
            layer.add(new Kinetic.Text({
                text: '0%',
                fontSize: 12,
                x: 25,
                y: 20,
                fill: '#000'
            }));

            stage.add(layer);

            var prevTime = 0;
            var anim = new Kinetic.Animation(function(frame) {
            var time = frame.time,
                timeDiff = frame.timeDiff,
                frameRate = frame.frameRate;

                // update stuff
                if (time - prevTime >= 1000) {
                    points.shift();
                    points.shift();
                    var last = points[points.length - 2];
                    for (var i = 0; i < points.length; i += 2) {
                        points[i] = points[i] - 20;
                    }
                    points.push(last);
                    points.push((Math.random() * 1000) % 360 + 20);

                    line.setPoints(points);

                    for (var i = 0; i < circs.length; i++) {
                        circs[i].setX( points[i * 2] );
                        circs[i].setY( points[i * 2 + 1]);
                    }

                    prevTime = time;
                    stage.draw();
                }
            }, layer);

            anim.start();
        }
    });

    return PlantDetailsView;
});