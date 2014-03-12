var IntersectionsCollectionView = Backbone.View.extend({

	// initialize()
	initialize: function(options){
		this.map = options.map;
		this.panelView = options.panelView;

		this.intersectionViews = new Array();

		// If there are intersections in the collection already, make views for them
		this.model.forEach(this.onAddedIntersection, this);
		// whenever an intersection is added to the model, we make the appropriate views for it.
		this.model.on('add', this.onAddedIntersection, this);

	},

	// onAddedIntersection()
	// creates an intersection view object and associates it with the added intersection model.
	onAddedIntersection: function(intersection){
		// make the intersection's view and connect them.
		var intersection_view = new IntersectionView({ model: intersection, map: this.map, panelView: this.panelView });
		this.intersectionViews.push(intersection_view);

		// introduce some fake data for testing
		intersection.set('volume', Math.random());
		intersection.set('performance', Math.random());
	},

	// called to iterate through the intersections and change the styles
	setIntersectionStyle: function(markerStyle){
		_.each(this.intersectionViews, function(intersection){
			intersection.setMarkerStyle(markerStyle);
		}, this);
	}
});