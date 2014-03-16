var IntersectionView = Backbone.View.extend({

	tagName: 'div', // need to look into this

	initialize: function(options){
		var self = this;
		this.model = options.model; // reference the intersection model
		this.map = options.map; // reference the map
		this.panelView = options.panelView;
		this.infoBoxView = options.infoBoxView;

		this.markerStyles = bootstrap.markerStyles;

		this.lat = this.model.get('lat');
		this.lng = this.model.get('long');
		this.LatLng = new google.maps.LatLng(this.lat, this.lng)
		this.title = this.model.get('name');

		this.marker = new google.maps.Marker({
			//map: this.map, // so it calls from render...
			position: this.LatLng,
			title: this.title,
			map: this.map,
			intersectionView: this
		});

		this.performanceGlyphTemplate = 'M cx cy m -r, 0 a r,r 0 1,0 d,0 a r,r 0 1,0 -d,0';
		this.flowGlyphTemplate = 'M cx cy m -r, 0 a r,r 0 1,0 d,0 a r,r 0 1,0 -d,0';

		this.performanceGlyph = {
			path: '',
		    fillColor: '',
		    fillOpacity: 0.8,
		    scale: 1
		};

		this.flowGlyph = {
			path: '',
		    fillColor: '',
		    fillOpacity: 0.8,
		    scale: 1
		};

		// google maps info window
		this.infoWindow = new google.maps.InfoWindow({
			content: this.title
		});


		// listeners, etc.
		google.maps.event.addListener(this.marker, 'click', function(event){
			this.intersectionView.onMarkerClick();
		});

		this.render();
	},

	render: function() {
	},

	remove: function(){
	},

	onMarkerClick: function(){ //pass this marker and model to the popup
		this.infoBoxView.setIntersection(this.marker, this.model);
	},

	setMarkerStyle: function(markerStyle){
		switch(markerStyle){
			case('bw_pin'):
				// size the glyph appropriately
				// var r = Math.ceil(Math.random() * 8);
				var r = 5.5;
				var d = 2*r;
				var cx = r;
				var cy = r;
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/r/g, r.toString());
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/d/g, d.toString());
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/cx/g, cx.toString());
				this.performanceGlyphTemplate = this.performanceGlyphTemplate.replace(/cy/g, cy.toString());

				// this.performanceGlyph['fillOpacity'] = Math.random();
				var capacity = Math.random();
				if(capacity < 0.95){
					this.performanceGlyph['fillColor'] = 'rgba(255, 255, 255, 0.8)';
				} else {
					this.performanceGlyph['fillColor'] = 'rgba(234, 20, 20, 1)';
				}

				this.performanceGlyph['strokeColor'] = 'rgba(0, 0, 0, 0)';

				this.performanceGlyph['path'] = this.performanceGlyphTemplate;

				this.marker.setOptions({ icon: this.performanceGlyph });
				break;

			case('performance_glyph'):
				// size the glyph appropriately
				var r = 14;
				var d = 2*r;
				var cx = r;
				var cy = r;

				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/r/g, r.toString());
				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/d/g, d.toString());
				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/cx/g, cx.toString());
				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/cy/g, cy.toString());

				this.flowGlyph['fillColor'] = 'rgba(28, 247, 64, 0.15)';
				this.flowGlyph['strokeColor'] = 'rgba(13, 139, 209, 0.07)';
				this.flowGlyph['strokeWeight'] = '14';
				
				this.flowGlyph['path'] = this.flowGlyphTemplate;

				this.marker.setOptions({ icon: this.flowGlyph });

				break;
			default:
				break;
		}
	},

	setMap: function(map){
		this.marker.setMap(map);
	}

});
