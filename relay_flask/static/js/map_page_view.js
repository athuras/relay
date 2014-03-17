var MapPageView = Backbone.View.extend({
	el: $('#map-page'),

	events: {
		'click .layer-view': 'layerSelected',
		'click .layer-btn': 'layerSelected',

		'click .map-layer-option': 'layerSelected'
	},

	initialize: function(options){
		// Add data from options
		this.intersectionsCollection = options.ic;

		// Get bootstrapped data
		this.mapOptions = bootstrap.mapOptions;
		this.mapStyles = bootstrap.mapStyles;
		this.activeLayer = bootstrap.activeLayer;
		this.heatmapStyles = bootstrap.heatmapStyles;

		// other data
		this.flowData = new Array();
		this.heatmapData = new Array();
		this.heatmapLayer = new google.maps.visualization.HeatmapLayer();
		this.heatmapLayer.setOptions(this.heatmapStyles);

		// Create our map object
		this.map = new google.maps.Map(document.getElementById('map'), bootstrap.mapOptions);

		// // Create a heatmap object
		// var heatmap = new google.maps.visualization.HeatmapLayer({
		// 	data: bootstrap.heatmapData
		// });

		// heatmap.setMap(map);

		// Create our info box
		this.infoBoxView = new InfoBoxView({mapPageView: this, map: this.map});

		// Make our panel View
		this.panelView = new PanelView();

		// Create our intersection Collection View
		this.intersectionsCollectionView = new IntersectionsCollectionView({model: this.intersectionsCollection, map: this.map, panelView: this.panelView, infoBoxView: this.infoBoxView});

		// Add listeners
		google.maps.event.addListener(this.map, 'click', $.proxy(function(e){
			this.mapClicked(e);
		}, this));

		$(document).keyup($.proxy(function(e){
			this.escapeKeyed(e);
		}, this));

		this.render();
	},

	render: function(){
		this.setActiveLayer(bootstrap.activeLayer);
	},

	mapClicked: function(e){
		console.log('map click registered');
		console.log(e);
		// I don't know why the details process causes another map click.
		if(this.infoBoxView.isOpen){
			this.infoBoxView.close();
		}
	},

	escapeKeyed: function(e){
		if(this.infoBoxView.isOpen){
			this.infoBoxView.close();
		}
		if(this.panelView.isExpanded){
			this.panelView.collapse();
		}
	},

	// handle clicks on the layers legend to change the layer
	layerSelected: function(e){
		// this.currentLayerLabel.removeClass('active');
		// var newActiveLayer = $(e.target).data('layer');
		this.setActiveLayer($(e.target).data('layer'));
	},

	// when someone selects a layer, we change the map and marker styles, and update the active layer reference
	setActiveLayer: function(layer){
		//remove active status
		_.forEach($('.map-layer-option'), function(layer){
			$(layer).removeClass('active')
		})
		//add active status to the appropriate label
		$('#'+layer+'-label').addClass('active');

		// remove any layer or markers from the map
		this.intersectionsCollectionView.setIntersectionMap(null);
		this.heatmapLayer.setMap(null);

		// Handle the different layer cases
		switch(layer){
			case('status-layer'):
				// set map and glyph styles
				this.map.setOptions({ styles: this.mapStyles['dark'] });
				this.intersectionsCollectionView.setIntersectionStyle( 'bw_pin' );
				// add them to the map
				this.intersectionsCollectionView.setIntersectionMap(this.map);
				break;
			case('flow-layer'):
				var points = new Array();

				//lat and long references
				var lat = intersection.get('lat');
				var lng = intersection.get('long');

				var flow = Math.floor(Math.random()*10); //get a flow number from the model

				// get one marker for each flow
				for(var i = 0; i < flow; i++){
					var olat = lat + (Math.random()-0.5)*0.1;
					var olng = lng + (Math.random()-0.5)*0.1;
					var m = new google.maps.LatLng(olat, olng);
					points.push(m);
				}

				_.each(points, function(p){
					var m = new google.maps.marker({
						position: p,

					});
				})

				// // set map and glyph styles
				// this.map.setOptions({ styles: this.mapStyles['dark'] });
				// this.intersectionsCollectionView.setIntersectionStyle( 'performance_glyph' );
				// // add them to the map
				// this.intersectionsCollectionView.setIntersectionMap(this.map);
				break;
			case('line-layer'):
				// set map and glyph styles
				this.map.setOptions({ styles: this.mapStyles['dark'] });
				this.intersectionsCollectionView.setIntersectionStyle( 'line_glyph' );
				// add them to the map
				this.intersectionsCollectionView.setIntersectionMap(this.map);
				break;
			case('flow-visualization'):
				// // heatmap array
				this.heatmapData = new Array();
				_.forEach(this.intersectionsCollection.models, function(intersection){
					var l = new google.maps.LatLng(intersection.get('lat'), intersection.get('long'));
					var wl = new Object({
						location: l,
						weight: Math.log(Math.random()*10+1) // should be a real number.
					});
					this.heatmapData.push(wl);
				}, this);
				this.heatmapLayer.setData(this.heatmapData);
				this.heatmapLayer.setMap(this.map);
			default:
				break;
		}
	},

	// when a user clicks show more in an intersection popup
	// close the popup, open the panel and populate w/ that intersections info
	showMore: function(model){
		this.infoBoxView.close();
		this.panelView.expand();
		this.panelView.showIntersectionDetails(model);
	},

	setActive: function(){

	},

	setInactive: function(){

	}

});