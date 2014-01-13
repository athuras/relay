var IntersectionView = Backbone.View.extend({

	tagName: 'div', // need to look into this

	initialize: function(options){
		var self = this;
		this.model = options.model; // reference the intersection model
		this.map = options.map; // reference the map

		this.lat = this.model.get('lat');
		this.lng = this.model.get('long');
		this.title = this.model.get('name');

		this.marker = new google.maps.Marker({
			//map: this.map, // so it calls from render...
			position: new google.maps.LatLng(this.lat, this.lng),
			title: this.title,
			map: this.map.getMap()
		});

		this.marker.infoWindow = new google.maps.InfoWindow({
			content: this.title
		});

		// listeners, etc.
		google.maps.event.addListener(this.marker, 'click', this.onMarkerClick);

		this.render();
	},

	onMarkerClick: function(){ // the marker is implicitly passed as the context
		this.infoWindow.open(this.map, this);
	},

	render: function() {
	},

	remove: function(){

	}
});