var getLayer = function(url,attrib) {
    return L.tileLayer(url, { maxZoom: 18, attribution: attrib });
};

var Layers = {
    stamen: { 
        toner:  'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png',   
        terrain: 'http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png',
        watercolor: 'http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png',
        attrib: 'Map data &copy;2013 OpenStreetMap contributors, Tiles &copy;2013 Stamen Design'
    },
    mapBox: {
        azavea:     'http://{s}.tiles.mapbox.com/v3/azavea.map-zbompf85/{z}/{x}/{y}.png',
        worldGlass:     'http://{s}.tiles.mapbox.com/v3/mapbox.world-glass/{z}/{x}/{y}.png',
        worldBlank:  'http://{s}.tiles.mapbox.com/v3/mapbox.world-blank-light/{z}/{x}/{y}.png',
        worldLight: 'http://{s}.tiles.mapbox.com/v3/mapbox.world-light/{z}/{x}/{y}.png',
        attrib: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery &copy; <a href="http://mapbox.com">MapBox</a>'
    }
};

var map = (function() {
    var selected = getLayer(Layers.mapBox.azavea,Layers.mapBox.attrib);

    var baseLayers = {
        "Azavea" : selected,
        "World Light" : getLayer(Layers.mapBox.worldLight,Layers.mapBox.attrib),
        "Terrain" : getLayer(Layers.stamen.terrain,Layers.stamen.attrib),
        "Watercolor" : getLayer(Layers.stamen.watercolor,Layers.stamen.attrib),
        "Toner" : getLayer(Layers.stamen.toner,Layers.stamen.attrib),
        "Glass" : getLayer(Layers.mapBox.worldGlass,Layers.mapBox.attrib),
        "Blank" : getLayer(Layers.mapBox.worldBlank,Layers.mapBox.attrib)
    };

    var m = L.map('map').setView([39.9886950160466,-75.1519775390625], 10);

    selected.addTo(m);

    m.lc = L.control.layers(baseLayers).addTo(m);

    $('#map').resize(function() {
        m.setView(m.getBounds(),m.getZoom());
    });

    return m;
})();

var travelTimes = (function() {
    var mapLayer = null;
    var vectorLayer = null;
    var opacity = 0.7;

    var duration = 10*60;
    var time = 8*60*60;

    return {
        setOpacity : function(o) {
            opacity = o;
            if(mapLayer) { 
                mapLayer.setOpacity(opacity); 
            }
        },
        setTime : function(o) {
            time = o;
            travelTimes.update();
        },
        setDuration : function(o) {
            duration = o;
            travelTimes.update();
        },
        update : function() {
            $.ajax({
                url: 'gt/travelshed/request',
                data: { latitude: startMarker.getLat(),
                        longitude: startMarker.getLng(),
                        time: time,
                        duration: duration,
                        colorRamp: colorRamps.getColorRamp(),
                        format: 'image/png' },
                dataType: "json",
                success: function(data) {
                    if (mapLayer) {
                        map.lc.removeLayer(mapLayer);
                        map.removeLayer(mapLayer);
                        mapLayer = null;
                    }
                    if (vectorLayer) {
                        map.lc.removeLayer(vectorLayer);
                        map.removeLayer(vectorLayer);
                        vectorLayer = null; 
                    }

                    if(data.token) {
                        token = data.token
                        mapLayer = new L.TileLayer.WMS("gt/travelshed/wms", {
                            token: token,
                            attribution: 'Azavea'
                        })


                        mapLayer.setOpacity(opacity);
                        mapLayer.addTo(map);
                        map.lc.addOverlay(mapLayer, "Travel Times");

                        $.ajax({
                            url: 'gt/travelshed/json',
                            data: { token: token },
                            success: function(data) {
                                if (vectorLayer) {
                                    map.lc.removeLayer(vectorLayer);
                                    map.removeLayer(vectorLayer);
                                    vectorLayer = null; 
                                }

                                vectorLayer = L.geoJson().addTo(map);
                                vectorLayer.addData(data); 
                            }
                        })
                        
                    }
                }
            });
        }
    }
})();

var startMarker = (function() {
    var lat = 40.0175
    var lng = -75.059

    var marker = L.marker([lat,lng], {
        draggable: true 
    }).addTo(map);
    
    marker.on('dragend', function(e) { 
        lat = marker.getLatLng().lat;
        lng = marker.getLatLng().lng;
        travelTimes.update();
    } );

    return {
        getMarker : function() { return marker; },
        getLat : function() { return lat; },
        getLng : function() { return lng; }
    }
})();

var colorRamps = (function() {
    var colorRamp = "blue-to-red";

    var makeColorRamp = function(colorDef) {
        var ramps = $("#color-ramp-menu");

        var p = $("#colorRampTemplate").clone();
        p.find('img').attr("src",colorDef.image);
        p.click(function() {
            $("#activeRamp").attr("src",colorDef.image);
            colorRamps.setColorRamp(colorDef.key);
        });
        if(colorDef.key == colorRamp) {
            $("#activeRamp").attr("src",colorDef.image);
        }
        p.show();
        ramps.append(p);
    }

    return { 
        bind: function() {
            $.ajax({
                url: 'gt/admin/colors',
                dataType: 'json',
                success: function(data) {
                    _.map(data.colors, makeColorRamp)
                }
            });
        },
        setColorRamp: function(key) { 
            colorRamp = key;
            travelTimes.update();
        },
        getColorRamp: function(key) { 
            return colorRamp;
        },
    };
})();

var opacitySlider = (function() {
    var opacitySlider = $("#opacity-slider").slider({
        value: 0.7,
        min: 0,
        max: 1,
        step: .02,
        slide: function( event, ui ) {
            travelTimes.setOpacity(ui.value);
        }
    });

    return {
        setOpacity: function(o) {
            opacitySlider.slider('value', o);
        }
    }
})();

var timeSlider = (function() {
    var opacitySlider = $("#opacity-slider").slider({
        value: 0.7,
        min: 0,
        max: 1,
        step: .02,
        slide: function( event, ui ) {
            travelTimes.setOpacity(ui.value);
        }
    });

    return {
        setOpacity: function(o) {
            opacitySlider.slider('value', o);
        }
    }
})();

var timeSlider = (function() {
    var slider = $("#time-slider").slider({
        value: 10*60*60,
        min: 0,
        max: 24*60*60,
        step: 10,
        change: function( event, ui ) {
            travelTimes.setTime(ui.value);
        }
    });

    return {
        setTime: function(o) {
            slider.slider('value', o);
        }
    }
})();

var durationSlider = (function() {
    var slider = $("#duration-slider").slider({
        value: 10*60,
        min: 0,
        max: 45*60,
        step: 1,
        change: function( event, ui ) {
            travelTimes.setDuration(ui.value);
        }
    });

    return {
        setDuration: function(o) {
            slider.slider('value', o);
        }
    }
})();

var setupSize = function() {
    var bottomPadding = 10;

    var resize = function(){
        var pane = $('#left-pane');
        var height = $(window).height() - pane.position().top - bottomPadding;
        pane.css({'height': height +'px'});

        var mapDiv = $('#map');
        var height = $(window).height() - mapDiv.offset().top - bottomPadding;
        mapDiv.css({'height': height +'px'});

        map.invalidateSize();
    };
    resize();
    $(window).resize(resize);
};

// On page load
$(document).ready(function() {
    setupSize();
    colorRamps.bind();
    travelTimes.update();
});
