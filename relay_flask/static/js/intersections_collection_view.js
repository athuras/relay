var IntersectionsCollectionView = Backbone.View.extend({

	// initialize()
	initialize: function(options){
		this.map = options.map;
		this.panelView = options.panelView;
		this.infoBoxView = options.infoBoxView;

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
		var intersection_view = new IntersectionView({ model: intersection, map: this.map, panelView: this.panelView, infoBoxView: this.infoBoxView });
		this.intersectionViews.push(intersection_view);

		// introduce some fake data for testing
		intersection.set('volume', Math.random());
		intersection.set('performance', Math.random());
		intersection.set({
			'status': 'OK',

			'currentState': 'EWT',
			'nextState': 'EWY',
			'nextStateTime': 12569537329,

			'flowVolumeRunningHour': [1000, 2000, 1000, 2000],

			'flowVolumeRunningDay': [10000, 20000, 10000, 20000],

			// for the short-term flow graph
			'flowVolumesShort': [ // flow volumes per second interval for 5 minutes
				[5,4,5,4],
				[5,5,5,5],
				[6,5,6,5]
			],

			'flowVolumesPredictionShort': [ // flow volumes per second interval for 5 minutes
				[5,4,5,4],
				[5,5,5,5],
				[6,5,6,5]
			],

			// for the long-term flow graph
			'flowVolumesLong': [// flow volumers per 5 minute interval for 24 hrs
				[50, 60, 50, 60],
				[50, 50, 50, 50],
				[60, 60, 60, 60],
				[50, 60, 60, 60]
			],

			'turningBiasRunningHour': [
				[0.7, 0.2, 0.0, 0.1],
				[0.7, 0.2, 0.0, 0.1],
				[0.7, 0.2, 0.0, 0.1],
				[0.7, 0.2, 0.0, 0.1]
			],

			'turningBiasRunningDay': [
				[0.7, 0.2, 0.0, 0.1],
				[0.7, 0.2, 0.0, 0.1],
				[0.7, 0.2, 0.0, 0.1],
				[0.7, 0.2, 0.0, 0.1]
			]
		});
	},

	// called to iterate through the intersections and change the styles
	setIntersectionStyle: function(markerStyle){
		_.each(this.intersectionViews, function(intersection){
			intersection.setMarkerStyle(markerStyle);
		}, this);
	},

	setIntersectionMap: function(map){
		_.each(this.intersectionViews, function(intersection){
			intersection.setMap(map);
		}, this);
	}
});