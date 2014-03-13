var MapPageView = Backbone.View.extend({
	el: $('#map-page'),

	events: {
		'click .layer-view': 'layerSelected',
		'click .layer-btn': 'layerSelected',
	},

	initialize: function(options){
		// Add data from options
		this.intersectionsCollection = options.ic;

		// Get bootstrapped data
		this.mapOptions = bootstrap.mapOptions;
		this.mapLayers = bootstrap.mapLayers;
		this.activeLayer = bootstrap.activeLayer;

		// Create our map object
		this.map = new google.maps.Map(document.getElementById('map'), bootstrap.mapOptions);

		// Create our info box
		this.infoBoxView = new InfoBoxView({mapPageView: this, map: this.map});

		// Make our panel View
		this.panelView = new PanelView();

		// Create our intersection Collection View
		this.intersectionsCollectionView = new IntersectionsCollectionView({model: this.intersectionsCollection, map: this.map, panelView: this.panelView, infoBoxView: this.infoBoxView});

		// Add listener
		google.maps.event.addListener(this.map, 'click', $.proxy(function(e){
			this.mapClicked(e);
		}, this));

		this.render();
	},

	render: function(){
	},

	mapClicked: function(e){
		console.log('map click registered');
		console.log(e);
		// I don't know why the details process causes another map click.
		// if(this.infoBoxView.isOpen){
		// 	this.infoBoxView.close();
		// }
		// if(this.panelView.isExpanded){
		// 	this.panelView.collapse();
		// }
	},

	// when someone selects a layer, we change the map and marker styles, and update the active layer reference
	layerSelected: function(layer){
		var newActiveLayer = layer.target.id;
		var mapStyle, markerStyle;
		switch(newActiveLayer){
			case('status-layer'):
				mapStyle = 'vintage';
				markerStyle = 'bw_pin';
				break;
			case('performance-layer'):
				mapStyle = 'dark';
				markerStyle = 'performance_glyph';
				break;
			default:
				break;
		}

		var newMapLayer = _.findWhere( this.mapLayers, {id: mapStyle } );
		this.map.setOptions({ styles: newMapLayer.styleArray });

		this.intersectionsCollectionView.setIntersectionStyle( markerStyle );

		this.$('#'+this.activeLayer).removeClass('active');
		this.$('#'+newActiveLayer).addClass('active');
		this.activeLayer = newActiveLayer;
	},

	// when a user clicks show more in an intersection popup
	// close the popup, open the panel and populate w/ that intersections info
	showMore: function(model){
		this.infoBoxView.close();
		this.panelView.expand();
		this.panelView.showIntersectionDetails(model);
	}

});