var MapView = Backbone.View.extend({

	// HTML object attacted to this view.
	el: $("#map"),

	// No defaults at this moment.
	defaults: {
	},

	// Initialize()
	// Creates the google map.
	initialize: function(options) {
		// model is referenced automatically if declared in the options

		// Set the OTHER map options
		this.mapOptions = {
			zoom: 9,
			mapTypeId: google.maps.MapTypeId.ROADMAP,
			center: new google.maps.LatLng(50,50),
			styles: this.model.get('activeMapStyle')
		};

		// Create the map
		this.map = new google.maps.Map(document.getElementById('map'), this.mapOptions);

		// Add listeners
		this.model.on('change:activeMapStyle', this.onMapStyleChange, this);
	},

	// getMap()
	//Public access to the gmaps javascript object.
	getMap: function(){
		return this.map;
	},

	// onMapStyleChange()
	// if the map style changes in the model, the map updates to reflect the new style
	onMapStyleChange: function(){
		this.map.setOptions({styles:this.model.get('activeMapStyle')});
	}
})