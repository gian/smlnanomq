#include <nmq.hpp>

typedef void * smlnmq_context_t;
typedef void * smlnmq_node_t;

extern "C"
{
	smlnmq_context_t sml_nmq_context (const char *fname) {
		nmq::context_t *ctx = new nmq::context_t(fname);

		return (smlnmq_context_t)ctx;
	}

	smlnmq_context_t sml_nmq_create (smlnmq_context_t ctx, unsigned int nodes, unsigned int size, unsigned int msg_size) {
		if(!((nmq::context_t*)ctx)->create(nodes,size,msg_size))
			return NULL;

		return ctx;
	}

	smlnmq_context_t sml_nmq_open (smlnmq_context_t ctx, unsigned int nodes, unsigned int size, unsigned int msg_size) {
		if(!((nmq::context_t*)ctx)->open(nodes,size,msg_size))
			return NULL;

		return ctx;
	}

	void sml_nmq_print (smlnmq_context_t ctx) {
		((nmq::context_t*)ctx)->print();
	}

	void sml_nmq_close (smlnmq_context_t ctx) {
		// Could consider deleting the file here...
		delete (nmq::context_t*)ctx;
	}

	smlnmq_node_t sml_nmq_node(smlnmq_context_t ctx, unsigned int nodeid) {
		nmq::node_t *node = new nmq::node_t(*((nmq::context_t*)ctx),nodeid);
		return (smlnmq_node_t)node;
	}

	int sml_nmq_send(smlnmq_node_t node, unsigned int to, void *msg, unsigned int len) {
		return ((nmq::node_t*)node)->send(to,msg,len);
	}

	int sml_nmq_sendnb(smlnmq_node_t node, unsigned int to, void *msg, unsigned int len) {
		return ((nmq::node_t*)node)->sendnb(to,msg,len);
	}

	int sml_nmq_recvfrom(smlnmq_node_t node, unsigned int from, void *msg, size_t *len) {
		return ((nmq::node_t*)node)->recv(from,msg,len);
	}

	int sml_nmq_recvfromnb(smlnmq_node_t node, unsigned int from, void *msg, size_t *len) {
		return ((nmq::node_t*)node)->recvnb(from,msg,len);
	}

	int sml_nmq_recv(smlnmq_node_t node, void *msg, size_t *len) {
		return ((nmq::node_t*)node)->recv(msg,len);
	}

	int sml_nmq_recvnb(smlnmq_node_t node, void *msg, size_t *len) {
		return ((nmq::node_t*)node)->recvnb(msg,len);
	}
}

