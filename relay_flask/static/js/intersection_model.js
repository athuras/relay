var IntersectionModel = Backbone.Model.extend({

	defaults: { // So we know what should be in there.
			// 'int_id': null,

			// 'lat': null,
			// 'long': null,

			// 'name': null,
			// 'type': null,
			// 'type_short': null,

			// //defaults for testing:
			// 'status': 'OK',
			// 'state': 'EWT',
			// 'nextState': 'EWC',
			// 'nextStateTime': '0:22',

			balloon: 'yes'
	},

	initialize: function() { // called automatically when created
		this.latLng = new google.maps.LatLng(this.lat, this.long);
	},
});