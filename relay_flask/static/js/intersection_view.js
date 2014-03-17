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

		this.performanceGlyphTemplate = 'M cx cy  m -r,0  a r,r 0 1,0 d,0 a r,r 0 1,0 -d,0';
		this.flowGlyphTemplate = 'M cx cy m -r, 0 a r,r 0 1,0 d,0 a r,r 0 1,0 -d,0';
		this.lineGlyphTemplate = 'M cx,cy  L xn,yn  M cx,cy  L xs,ys  M cx,cy  L xe,ye  M cx,cy  L xw, yw';

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

		this.lineGlyph = {
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

				if(this.model.get('status') == 'OK'){
					this.performanceGlyph['fillColor'] = 'rgba(255, 255, 255, 0.8)';
				} else {
					this.performanceGlyph['fillColor'] = 'rgba(234, 20, 20, 1)';
				}

				this.performanceGlyph['strokeColor'] = 'rgba(0, 0, 0, 0)';

				this.performanceGlyph['path'] = this.performanceGlyphTemplate;

				this.marker.setOptions({ icon: this.performanceGlyph });
				break;

			case('performance_glyph'):
				//size the glyph appropriately

				var setter = Math.random()*10;

				var r = 20;
				var d = 2*r;
				var cx = r;
				var cy = r;

				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/r/g, r.toString());
				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/d/g, d.toString());
				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/cx/g, cx.toString());
				this.flowGlyphTemplate = this.flowGlyphTemplate.replace(/cy/g, cy.toString());

				this.flowGlyph['fillColor'] = 'rgba(28, 247, 64, 0.1)';
				this.flowGlyph['strokeColor'] = 'rgba(13, 139, 209, 0.07)';
				this.flowGlyph['strokeWeight'] = '14';
				
				this.flowGlyph['path'] = this.flowGlyphTemplate;

				this.marker.setOptions({ icon: this.flowGlyph });

				break;

			case('line_glyph'):

				var randNorth = Math.pow(Math.random() * 9.5, 2) * (-1);
				// var randSouth = Math.random()*15;
				// var randEast = Math.random()*10;
				// var randWest = Math.random()*(-25);

				var r = 0;
				var xn = 0;
				var yn = randNorth;
				var xs = 0;
				var ys = 5;
				var xe = 5;
				var ye = 0;
				var xw = -5;
				var yw = 0;
				var cx = r;
				var cy = r;

				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/r/g, r.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/xn/g, xn.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/yn/g, yn.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/xs/g, xs.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/ys/g, ys.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/xe/g, xe.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/ye/g, ye.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/xw/g, xw.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/yw/g, yw.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/cx/g, cx.toString());
				this.lineGlyphTemplate = this.lineGlyphTemplate.replace(/cy/g, cy.toString());

				// if (randNorth > -8) {
				// 	this.lineGlyph['strokeColor'] = 'rgba(255, 255, 255, 0.5)';
				// } else if (randNorth < -13) {
				// 	this.lineGlyph['strokeColor'] = 'rgba(28, 247, 64, 0.5)';
				// } else {
				// 	this.lineGlyph['strokeColor'] = 'rgba(13, 139, 209, 0.5)';
				// }


				// this.lineGlyph['fillColor'] = 'rgba(28, 247, 64, 0)';
				this.lineGlyph['strokeColor'] = 'rgba(13, 139, 209, 0.9)';
				this.lineGlyph['strokeWeight'] = '1.5';
				
				this.lineGlyph['path'] = this.lineGlyphTemplate;

				this.marker.setOptions({ icon: this.lineGlyph });

				break;
			default:
				break;
		}
	},

	setMap: function(map){
		this.marker.setMap(map);
	}

});
