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

		// Create our map object
		this.map = new google.maps.Map(document.getElementById('map'), bootstrap.mapOptions);

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
		this.setActiveLayer('status-layer');
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
		$('#'+layer+'-label').addClass('active');

		var mapStyle, markerStyle;
		switch(layer){
			case('status-layer'):
				mapStyle = 'dark';
				markerStyle = 'bw_pin';
				break;
			case('flow-layer'):
				mapStyle = 'dark';
				markerStyle = 'performance_glyph';
				break;
			default:
				break;
		}

		this.map.setOptions({ styles: this.mapStyles[mapStyle] });

		this.intersectionsCollectionView.setIntersectionStyle( markerStyle );
	},

	// when a user clicks show more in an intersection popup
	// close the popup, open the panel and populate w/ that intersections info
	showMore: function(model){
		this.infoBoxView.close();
		this.panelView.expand();
		this.panelView.showIntersectionDetails(model);
	}

});