var MapView = Backbone.View.extend({

	// HTML object attacted to this view.
	el: $("#map"),

	// No defaults at this moment.
	defaults: {
	},

	// Initialize()
	// Creates the google map.
	initialize: function() {
		this.mapOptions = {
			zoom: 9,
			mapTypeId: google.maps.MapTypeId.ROADMAP,
			center: new google.maps.LatLng(50,50)
		};
		this.map = new google.maps.Map(document.getElementById('map'), this.mapOptions);
	},

	// getMap()
	//Public access to the gmaps javascript object.
	getMap: function(){
		return this.map;
	}
})