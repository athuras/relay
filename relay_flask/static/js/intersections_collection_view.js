var IntersectionsCollectionView = Backbone.View.extend({

	// initialize()
	initialize: function(options){
		this.model = options.model;
		this.map = options.map;

		// whenever an intersection is added to the model, we make the appropriate views for it.
		this.model.on('add', this.onAddedIntersection, this);
	},

	// onAddedIntersection()
	// creates an intersection view object and associates it with the added intersection model.
	onAddedIntersection: function(intersection){
		// make the intersection's view and connect them.
		var intersection_view = new IntersectionView({model: intersection, map: this.map});
	}
});