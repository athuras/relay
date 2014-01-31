var AppView = Backbone.View.extend({

	// HTML object associated with AppView
	el: $('#app'),

	// Other references:
	header: $('#header'),
	legendBtn: $('#legend-btn'),
	legendBox: $('#legend-box'),

	events: {
		'click .layer-view': 'layerSelected',

		'click #legend-btn': 'legendButtonClicked'
	},

	// initialize()
	// starts the app
	initialize: function() {

		// Get bootstrapped data
		this.mapOptions = bootstrap.mapOptions;
		this.mapLayers = bootstrap.mapLayers;
		this.activeLayer = bootstrap.activeLayer;

		// Create the layer views, and attach listeners
		this.layerList = new Array();
		var layerDom = '<p class="layer-view" id="<%= id %>"> <%= title %> </p>';
		_.each(this.mapLayers, function(layer){
			var layerView = _.template(layerDom, layer);
			this.layerList.push(layerView);
			this.$('#map-layer-container').append(layerView);
		}, this);

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

	// when someone selects a layer, we change the map and marker styles, and update the active layer reference
	layerSelected: function(layer){
		var newActiveLayer = layer.target.id;
		var newLayer = _.findWhere( this.mapLayers, {id: layer.target.id } );
		this.map.setOptions({ styles: newLayer.styleArray });
		this.intersectionsCollectionView.setIntersectionStyle( newLayer.markerStyle );
		this.$('#'+this.activeLayer).removeClass('active');
		this.$('#'+newActiveLayer).addClass('active');
		this.activeLayer = newActiveLayer;
	},

	// functions for handling legend selections
	legendButtonClicked: function(){
		// Show the legend options
		switch(this.legendBox.css('display')){
			case('none'):
				this.legendBox.css('display', 'block');
				break;
			case('block'):
				this.legendBox.css('display', 'none');
				break;
			default:
				break;
		}
	},




	render: function(){

	}
})
