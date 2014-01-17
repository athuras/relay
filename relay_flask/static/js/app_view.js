var AppView = Backbone.View.extend({

	// HTML object associated with AppView
	el: $('#app'),

	// initialize()
	// starts the app
	initialize: function() {
		// Make the map
		this.mapModel = new MapModel;
		this.map = new MapView({model: this.mapModel});

		// Make the intersection store
		this.intersectionsCollection = new IntersectionsCollection({
			map: this.map
		});
		this.intersectionsCollectionView = new IntersectionsCollectionView({
			model: this.intersectionsCollection,
			map: this.map
		});

		// Make the MV for the layer contoller
		// this.mapStyleCollection = new MapStyleCollection({
		// 	map: this.MapView.getMap
		// });
		// this.mapStyleCollectionView = new MapStyleCollectionView({
		// 	model:this.MapStyleCollection
		// });

		// Make the MV for the panel controller
		// this.intersectionDetailsModel = new IntersectionDetailsModel();
		// this.IntersectionDetailsModelView = new IntersectionDetailsModelView();

		// Initial fetch of the intersections
		this.intersectionsCollection.fetch();
	}
})