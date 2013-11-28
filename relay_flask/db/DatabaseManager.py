import sqlite3

class DatabaseManager(object):
    '''Singleton Container for the various databases we'll be using'''
    _relay_db_info = {'relay_main': 'relay.db'}

    def __init__(self, **kwargs):
        db_info = kwargs.get('db_info', DatabaseManager._relay_db_info)
        self.connections = {}
        for name, f in db_info.iteritems():
            self.connections[name] = sqlite3.connect(f)

    def prepare_cursor(self, db_name, query, options):
        '''Returns executed cursor'''
        conn = self.connections[db_name]
        cur = conn.cursor()
        if sqlite3.complete_statement(query):
            try:
                query = query.strip()
                cur.execute(query, options)
            except sqlite3.Error as e:
                print 'Bad news: ', e.args[0]
        else:
            raise ValueError('""%s"" is not a valid SQL Statement' % query)

        return cur

    def iter_query(self, db_name, query, options, gen_dict):
        '''Returns an iterator or dict over the query results'''
        cur = self.prepare_cursor(db_name, query, options)
        if gen_dict:
            for line in cur:
                yield dict((cur.description[i][0], value) for i, value in 
                    enumerate(line))
        else:
            for line in cur:
                yield line

    def query(self, db_name, query, options, gen_dict):
        '''Returns entire result set, don't select *!!!'''
        cur = self.prepare_cursor(db_name, query, options)
        results = cur.fetchall()

        if gen_dict:
            return [dict((cur.description[i][0], value) for i, value in 
            enumerate(row)) for row in results]
        else:
            return results

    def close_all(self):
        '''Close all open Database Connections'''
        for k in self.connections:
            self.connections[k].close()
