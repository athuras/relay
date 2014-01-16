var MapModel = Backbone.Model.extend({
	// the model won't attach to anything backend yet
	// stores map styles, layers, etc. so the view has something to listen to.
	defaults: {
		'mapOptions': null,
		'mapStyles': null
	},

	initialize: function(){

		// We set the map styles and map options to be attributes
		this.set('mapStyles', [
			{
				'title': 'light',
				'styleArray': [
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
				]
			},
			{
				'title': 'dark',
				'styleArray': [
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
			  ]
			}
		]);

		this.set('mapOptions', {
			zoom: 8,
			mapTypeId: google.maps.MapTypeId.ROADMAP,
			center: new google.maps.LatLng(42.7,-79.4)
		});

	}

});