var MapStyleCollectionView = Backbone.View.extend({

	el: $('#map-style-container'),

	initialize: function(options){
		// Model is automatically passed
		this.map = options.map;

		this.render();
	},

	render: function(){
		this.model.each(this.onMapStyleAdded, this);
	},

	// onStyleAdded()
	// When a mapStyleModel is added to the collection, we make a view for it
	onMapStyleAdded: function(mapStyle){
		var mapStyleView = new MapStyleView({model: mapStyle});
		this.$el.append(mapStyleView.render().el);
	}

});