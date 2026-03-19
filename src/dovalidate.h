#ifndef DOVALIDATE_H
#define DOVALIDATE_H

#include "search.h"
#include "dcongraph.h"
#include "derivation.h"


class dovalidate: public search {
public:
  dovalidate(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb, const bool& valid);
  int md_s, md_p, md_t, tr, sb, trsb;
  char md_sym;
  bool md;
  dcongraph* g;
  virtual void add_distribution(distr& nquery);
  virtual void add_known(const int& a, const int& b, const int& c, const int& d, const Rcpp::IntegerMatrix& mat);
  virtual distr& next_distribution(const int& i);
  void assign_candidate(distr& required);
  bool check_trivial();
  bool separation_criterion();
  int rule_limit(const int& ruleid, const unsigned int& z_size);
  void set_target(const int& a, const int& b, const int& c, const int& d);
  void set_graph(dcongraph* g_);
  void set_options(const std::vector<int>& rule_vec);
  void set_labels(const Rcpp::StringVector& lab);
  void set_md_symbol(const char& mds);
  bool is_primitive(const bool& pa1_primitive, const bool& pa2_primitive, const int& ruleid);
  std::string derive_formula(distr& dist);
  std::string dec_to_text(const int& dec, const int& enabled) const;
  std::string to_string(const p& pp) const;
  bool valid_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const bool& primi) const;
  bool valid_rule_with_z(const int& ruleid, const int& z, const int& allowed_rule, const int& allowed_z) const;
  void apply_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const int& z);
  void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found, const int& z);
  path derive_new_path(const path& pat2, const path& pat1,  const int& ruleid, const int& z);
  void get_ruleinfo(const int& ruleid, const int& y, const int& xw, const int& x, const int& d, const int& z);
  void get_candidate(distr& required, const int& req);
  void enumerate_candidates();
  virtual ~dovalidate();
};

class dovalidate_heuristic: public dovalidate {
public:
  struct comp_distr {
    bool operator()(distr const * d1, distr const * d2) {
      return d1->score < d2->score;
    }
  };
  dovalidate_heuristic(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb, const bool& valid);
  void add_distribution(distr& nquery);
  void add_known(const int& a, const int& b, const int& c, const int& d);
  distr& next_distribution(const int& j);
  virtual ~dovalidate_heuristic();
private:
  int compute_score(const p& pp) const;
  int compute_score_md(const p& pp) const;
  std::priority_queue<distr*, std::vector<distr*>, comp_distr> Q;
};

#endif
