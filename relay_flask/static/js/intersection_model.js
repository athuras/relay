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