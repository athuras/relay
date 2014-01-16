var MapStyleCollection = Backbone.Collection.extend({

	model: MapStyleModel,

	initialize: function(){
		this.add([
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
	},

	setActive: function(newStyle){
		this.trigger('activeChange', newStyle);
	}

});