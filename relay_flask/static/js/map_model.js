var MapModel = Backbone.Model.extend({
	// the model won't attach to anything backend yet
	// stores map styles, layers, etc. so the view has something to listen to.
	defaults: {
		"_mapStyleLight": [
		  {
		    featureType: "all",
		    stylers: [
		      { saturation: -80 }
		    ]
		  },{
		    featureType: "road.arterial",
		    elementType: "geometry",
		    stylers: [
		      { hue: "#00ffee" },
		      { saturation: 50 }
		    ]
		  },{
		    featureType: "poi.business",
		    elementType: "labels",
		    stylers: [
		      { visibility: "off" }
		    ]
		  }
		],

		"_mapStyleDark": [
		  {
		    stylers: [
		      { hue: "#00ffe6" },
		      { saturation: -20 }
		    ]
		  },{
		    featureType: "road",
		    elementType: "geometry",
		    stylers: [
		      { lightness: 100 },
		      { visibility: "simplified" }
		    ]
		  },{
		    featureType: "road",
		    elementType: "labels",
		    stylers: [
		      { visibility: "off" }
		    ]
		  }
		],

		"activeMapStyle": null

	},

	initialize: function(){
		// set default map style
		this.set('activeMapStyle', this.get('_mapStyleLight'));
	}

});