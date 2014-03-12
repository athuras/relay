var MapPageView = Backbone.View.extend({
	el: $('#map-page'),

	events: {
		'click .layer-view': 'layerSelected',
		'click .layer-btn': 'layerSelected',
	},

	initialize: function(){
		// Get bootstrapped data
		this.mapOptions = bootstrap.mapOptions;
		this.mapLayers = bootstrap.mapLayers;
		this.activeLayer = bootstrap.activeLayer;

		// Create our map object
		this.map = new google.maps.Map(document.getElementById('map'), bootstrap.mapOptions );

		// Make our panel View
		this.panelView = new PanelView();

		// Create our intersection Collection and View
		this.intersectionsCollection = new IntersectionsCollection();
		this.intersectionsCollectionView = new IntersectionsCollectionView({model: this.intersectionsCollection, map: this.map, panelView: this.panelView });


		// Initial fetch of the intersections
		this.intersectionsCollection.fetch();

		this.render();

	},

	render: function(){
		//select an initial map style: for now it's status
		setTimeout(function(){
			$('#status-layer').click();
		}, 200);
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

});