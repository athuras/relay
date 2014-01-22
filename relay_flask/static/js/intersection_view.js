var IntersectionView = Backbone.View.extend({

	tagName: 'div', // need to look into this

	initialize: function(options){
		var self = this;
		this.model = options.model; // reference the intersection model
		this.map = options.map; // reference the map
		this.panelView = options.panelView;

		this.markerStyles = bootstrap.markerStyles;

		this.lat = this.model.get('lat');
		this.lng = this.model.get('long');
		this.title = this.model.get('name');

		this.marker = new google.maps.Marker({
			//map: this.map, // so it calls from render...
			position: new google.maps.LatLng(this.lat, this.lng),
			title: this.title,
			map: this.map,
			intersectionView: this
		});

		this.infoWindow = new google.maps.InfoWindow({
			content: this.title
		});

		// listeners, etc.
		google.maps.event.addListener(this.marker, 'click', function(event){
			this.intersectionView.onMarkerClick();
		});

		google.maps.event.addListener(this.marker, 'mouseover', function(event){
			this.intersectionView.onHover();
		});

		google.maps.event.addListener(this.marker, 'mouseout', function(event){
			this.intersectionView.onHoverOut();
		});

		this.render();
	},

	render: function() {
	},

	remove: function(){
	},

	onMarkerClick: function(){ // the marker is implicitly passed as the context
		// this.infoWindow.open(this.map, this);
		this.panelView.showIntersectionDetails(this.model);
	},

	onHover: function(){
		this.infoWindow.open(this.map, this.marker);
	},

	onHoverOut: function(){
		this.infoWindow.close();
	},

	setMarkerStyle: function(styleId){
		this.marker.setOptions( this.markerStyles[styleId].options );
	}



});
