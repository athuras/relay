// function Intersection(geom, title) {
// 	// Geo properties
// 	this.geom = geom;

// 	// State properties
// 	this.state = new String();
// 	this.status = new String)();

// 	// Metric properties
// 	this.volume = new Array();
// 	this.performance=  new Array();

// 	// Aesthetic properties
// 	this.title = new String(title);
// }

var IntersectionModel = Backbone.Model.extend({
	defaults: { // So we know what should be in there.
		int_id: null,
		lat: null,
		long: null,
		name: null,
		type: null,
		type_short: null
	},

	initialize: function() { // called automatically when created

	}
});