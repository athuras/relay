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

		this.performanceGlyphTemplate = 'M cx cy m -r, 0 a r,r 0 1,0 d,0 a r,r 0 1,0 -d,0';
		this.performanceGlyph = {
			path: '',
		    fillColor: '',
		    fillOpacity: 0.8,
		    scale: 1
		};

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

	setMarkerStyle: function(markerStyle){
		switch(markerStyle){
			case('bw_pin'):
				this.marker.setOptions(this.markerStyles['bw_pin'].options);
				break;
			case('performance_glyph'):
				// size the glyph appropriately
				var r = Math.ceil(Math.random() * 20);
				var d = 2*r;
				var cx = r;
				var cy = r;
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/r/g, r.toString());
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/d/g, d.toString());
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/cx/g, cx.toString());
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/cy/g, cy.toString());

				this.performanceGlyph['fillOpacity'] = Math.random();
				var capacity = Math.random();
				if(capacity < 0.3){
					this.performanceGlyph['fillColor'] = 'green';
				} else if (capacity < 0.6){
					this.performanceGlyph['fillColor'] = 'yellow';
				} else {
					this.performanceGlyph['fillColor'] = 'red';
				}

				this.performanceGlyph['path'] = this.performanceGlyphTemplate;

				this.marker.setOptions({ icon: this.performanceGlyph });
				break;
			default:
				break;
		}
	}

});
