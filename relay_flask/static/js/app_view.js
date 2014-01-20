var AppView = Backbone.View.extend({

	// HTML object associated with AppView
	el: $('#app'),

	// initialize()
	// starts the app
	initialize: function() {

		// Make the intersection store
		this.mapModel = new MapModel;
		this.intersectionsCollection = new IntersectionsCollection({
		});
		this.mapStyleCollection = new MapStyleCollection({
		});

		// bootstrap the models and collections
		this.mapStyleCollection.reset(bootstrap.mapLayers);

		this.mapView = new MapView({
			model: this.mapModel,
			mapStyleCollection: this.mapStyleCollection
		});
		this.intersectionsCollectionView = new IntersectionsCollectionView({
			model: this.intersectionsCollection,
			map: this.mapView
		});
		this.mapStyleCollectionView = new MapStyleCollectionView({
			model: this.mapStyleCollection
		});

		// Initial fetch of the intersections
		this.intersectionsCollection.fetch();
	}
})
