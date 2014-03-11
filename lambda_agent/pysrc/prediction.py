class Prediction(object)

    Stamps_Thresh = 1000;

    def __init__(node, period, stamps_thresh, edge_length):
        # when node is initialized generate initial guesses of TD, SPD, etc.
        est_time = avg_speed(node.id) * edge_length
        Stamps_Thresh = stamps_thresh

    def build_departures(car_stamp):
        d_stamps = [for item in d_stamps if item >= Stamps_Thresh]
        d_stamps.append(car_stamp[1])

    def build_arrivals(car_stamp):
        a_stamps = [for item in a_stamps if item >= (Stamps_Thresh + est_time)]
        a_stamps.append(car_stamp[1])

    def make_pairs():
        # given two streams of data: Bi's and Aj (all entering nodes from downstream
        # plus entering point of interest) match data points to form time_delay. This
        # tells how long it has taken to move from one intersection to the next

        # we first need training data to form an estimate of SPD on edge, so we can 
        # infer this data

        # use time stamps of each data point and delta for TD

        #traffic_tstamp:
            # node_id (incoming node_id)
            # timestamp (exit_time)
            # meta (some other data)

        est_time = avg_speed(some_id) * edge_length

        t_delays.append(d_evt - a_evt)

    def get_time_delay(node):
        # TODO: make this a learned parameter
        return node.length * node.speed_limit

    def avg_speed(node_id):
        return 0
        # given a node id, what is the predicted speed for this road at the given time
            # --> USE THE MEAN OF THE LEARNED SPD

    # -----------
    # temp_methods
    def get_downstream_nodes(node_id):
        # (id, current_behaviour_id, incoming edge ids, outgoing edge ids)
        return ((1, 2, (1, 2, 3, 4), (5, 6, 7, 8)), (2, 3, (1, 2, 3, 4), \
            (5, 6, 7, 8)), (3, 3, (1, 2, 3, 4), (5, 6, 7, 8)))

    def get_bhvr_probs(behaviour):
        return ((0, 1, 0.1), (0, 2, 0.6), (0, 3, 0.3), (3, 2, 1), (1, 0, 1), \
            (2, 1, 0.2), (2, 0, 0.7), (2, 3, 0.1))

    def get_downstream_counts(d_nodes):
        # ids = tuple(node[0] for node in d_nodes)
        # send to erlang to get counts
        # does some stuff with time delays and edges to get the count of how many
        # cars reach a certain downstream node
        return ((0, 8.0), (1, 4.0), (2, 9.0), (3, 4.0))

    def get_served_counts(nodes):
        return ((0, 10.0), (1, 2.0), (2, 9.0), (3, 4.0))

    # reformatting tuples to matrices
    def create_bhvr_prob_mtx(bhvr_past):
        bhvr_prob_mtx = np.zeros([4,4])
        for edge in bhvr_past:
            bhvr_prob_mtx[edge[0], edge[1]] = edge[2]

        return bhvr_prob_mtx

    def create_counts_mtx(counts):
        return np.array([item[1] for item in counts])


    # updating behaviour probabilities
    def update_behaviour_stats(node, bhvr):
        # NEED:
        #   - get all neighbouring nodes
        #   - some way to tally counts if doing timing method
        #   - return tuple -> ((inner_id, n_id, count), ...)
        neighbour_data = get_neighbour_data(node[0])

        # how many obs there are at neighbouring inlets
        # NEED:
        #   - some way to tally these; by time, # occurences....
        #   - return a tuple neighbour refs and counts -> ((id, count),...)
        #neighbour_counts = get_neighbour_counts(neighbours)

        # get the count at each inlet of current node
        # NEED:
        #   - some way to tally these
        #   - use this plus dstream refs to get probs
        #   - tuple: ((in_id, count), ...)
        counts = get_counts(node[0])

        # pull up the old behaviour likelihoods
        # will be a tuple: ((in_id, out_id, prob), ...)
        bhvr_past = get_bhvr_probs(bhvr)

    def calculate_likelihoods(bhvr, bhvr_past, counts, neighbour_data):
        # bhvr = ((0,1), (0,2), ...) -> all edges "on"
        # bhvr_past = ((0, 1, 0.4), (0, 2, 0.5), ...) -> old probs
        # count = ((0, 10), (1, 0), ...) -> count of events
        # neighbour_data = (inner_id, n_id, count),...

        # may not need bhvr here, implicit in bhvr_past

        # remove secondary node cars seen at nbrs
        est_cars -= (q_len_beg - q_len_end)

        new_probs = tuple((edge[0], ) for edge in bhvr_past)

        (count[1] * bhvr_past[2], neighbour_data[2])

        for edge in bhvr:
            # matchs nbr node data for inlet edge
            nbr = tuple(nbr for nbr in neighbour_data if nbr[0] == edge[1])
            nbr_count = nbr[2]

             = filter(lambda x: x[0] == edge[0], count)[0][1]

    def calculate_bhvr_new(bhvr_arr, counts, nbr_counts):
        # calculates proportions of cars served during the bevhiours cycle
        # along each edge

        # multiplies by bhvr mask to ensure only allowable paths calc'd
        new_bhvr_arr = [nbr_counts / i for i in counts] \
            * (bhvr_arr > 0)

        new_bhvr_arr[new_bhvr_arr >= 1] = 1

        return new_bhvr_arr

    def update_edge_probs(bhvr_arr, counts, nbr_counts):
        alpha = 0.2
        Q_new = calculate_bhvr_new(bhvr_arr, counts, nbr_counts)
        
        return (1-alpha) * bhvr_arr + alpha * Q_new

    def update_outlet_count(bhvr_past, counts, nbr_counts):
        # removes counts from "secondary" (one inlet-outlet) nodes in neighbor counts
        new_nbr_counts = np.copy(nbr_counts)

        scnd_in, scnd_out = np.where(bhvr_arr==1.0)

        for i in range(len(scnd_in)):
            new_nbr_counts[scnd_out[i]] = 
                nbr_counts[scnd_out[i]] - counts[scnd_in[i]]

        return new_nbr_counts













