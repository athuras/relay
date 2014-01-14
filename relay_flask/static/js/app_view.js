var AppView = Backbone.View.extend({

	// HTML object associated with AppView
	el: $("#app"),

	// initialize()
	// starts the app
	initialize: function() {
		// create the key app variables
		this.mapModel = new MapModel;
		this.map = new MapView({model: this.mapModel});
		this.intersectionsCollection = new IntersectionsCollection({map: this.map});
		this.intersectionsCollectionView = new IntersectionsCollectionView({model: this.intersectionsCollection, map: this.map});

		// Initial fetch of the intersections
		this.intersectionsCollection.fetch();
	}
})