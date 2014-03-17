var IntersectionsCollectionView = Backbone.View.extend({

	intervalFrequency: 10000,

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

		// this.render();
		this.startInterval();
	},

	render: function(){

	},

	// onAddedIntersection()
	// creates an intersection view object and associates it with the added intersection model.
	onAddedIntersection: function(intersection){
		// make the intersection's view and connect them.
		var intersection_view = new IntersectionView({ model: intersection, map: this.map, panelView: this.panelView, infoBoxView: this.infoBoxView });
		this.intersectionViews.push(intersection_view);

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
	},

 	startInterval: function(){
 		this.interval = setInterval(function(icv){
 			icv.model.update();
 		}, this.intervalFrequency, this);
 	},

 	stopInterval: function() {
 		clearInterval(this.interval);
 	}
});