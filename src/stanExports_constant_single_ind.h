// Generated by rstantools.  Do not edit by hand.

/*
    rmot is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    rmot is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with rmot.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.32.2
#include <stan/model/model_header.hpp>
namespace model_constant_single_ind_namespace {
using stan::model::model_base_crtp;
using namespace stan::math;
stan::math::profile_map profiles__;
static constexpr std::array<const char*, 45> locations_array__ =
  {" (found before start of program)",
  " (in 'string', line 22, column 2 to column 24)",
  " (in 'string', line 23, column 2 to column 25)",
  " (in 'string', line 25, column 2 to column 35)",
  " (in 'string', line 51, column 2 to column 20)",
  " (in 'string', line 52, column 2 to column 24)",
  " (in 'string', line 56, column 6 to column 25)",
  " (in 'string', line 55, column 23 to line 57, column 5)",
  " (in 'string', line 55, column 4 to line 57, column 5)",
  " (in 'string', line 63, column 6 to column 56)",
  " (in 'string', line 62, column 11 to line 64, column 5)",
  " (in 'string', line 60, column 6 to column 70)",
  " (in 'string', line 61, column 6 to column 43)",
  " (in 'string', line 59, column 17 to line 62, column 5)",
  " (in 'string', line 59, column 4 to line 64, column 5)",
  " (in 'string', line 53, column 19 to line 65, column 3)",
  " (in 'string', line 53, column 2 to line 65, column 3)",
  " (in 'string', line 29, column 13 to column 18)",
  " (in 'string', line 29, column 2 to column 20)",
  " (in 'string', line 33, column 6 to column 25)",
  " (in 'string', line 32, column 23 to line 34, column 5)",
  " (in 'string', line 32, column 4 to line 34, column 5)",
  " (in 'string', line 37, column 6 to column 70)",
  " (in 'string', line 36, column 17 to line 38, column 5)",
  " (in 'string', line 36, column 4 to line 38, column 5)",
  " (in 'string', line 30, column 19 to line 39, column 3)",
  " (in 'string', line 30, column 2 to line 39, column 3)",
  " (in 'string', line 41, column 2 to column 44)",
  " (in 'string', line 44, column 2 to column 48)",
  " (in 'string', line 45, column 2 to column 31)",
  " (in 'string', line 47, column 2 to column 37)",
  " (in 'string', line 13, column 2 to column 12)",
  " (in 'string', line 14, column 13 to column 18)",
  " (in 'string', line 14, column 2 to column 20)",
  " (in 'string', line 15, column 16 to column 21)",
  " (in 'string', line 15, column 2 to column 23)",
  " (in 'string', line 16, column 12 to column 17)",
  " (in 'string', line 16, column 2 to column 19)",
  " (in 'string', line 17, column 2 to column 15)",
  " (in 'string', line 51, column 13 to column 18)",
  " (in 'string', line 52, column 17 to column 22)",
  " (in 'string', line 5, column 4 to column 16)",
  " (in 'string', line 4, column 20 to line 6, column 3)",
  " (in 'string', line 8, column 4 to column 31)",
  " (in 'string', line 7, column 46 to line 9, column 3)"};
template <typename T0__,
          stan::require_all_t<stan::is_stan_scalar<T0__>>* = nullptr>
stan::promote_args_t<T0__> DE(const T0__& beta, std::ostream* pstream__);
template <typename T0__, typename T1__, typename T2__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>,
                              stan::is_stan_scalar<T2__>>* = nullptr>
stan::promote_args_t<T0__, T1__, T2__>
size_step(const T0__& y, const T1__& beta, const T2__& time, std::ostream*
          pstream__);
template <typename T0__, stan::require_all_t<stan::is_stan_scalar<T0__>>*>
stan::promote_args_t<T0__> DE(const T0__& beta, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__>;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 41;
    return beta;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T0__, typename T1__, typename T2__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>,
                              stan::is_stan_scalar<T2__>>*>
stan::promote_args_t<T0__, T1__, T2__>
size_step(const T0__& y, const T1__& beta, const T2__& time, std::ostream*
          pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__, T1__, T2__>;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 43;
    return (y + (DE(beta, pstream__) * time));
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
#include <stan_meta_header.hpp>
class model_constant_single_ind final : public model_base_crtp<model_constant_single_ind> {
private:
  int n_obs;
  std::vector<double> y_obs;
  std::vector<int> obs_index;
  std::vector<double> time;
  double y_0_obs;
public:
  ~model_constant_single_ind() {}
  model_constant_single_ind(stan::io::var_context& context__, unsigned int
                            random_seed__ = 0, std::ostream*
                            pstream__ = nullptr) : model_base_crtp(0) {
    int current_statement__ = 0;
    using local_scalar_t__ = double;
    boost::ecuyer1988 base_rng__ =
      stan::services::util::create_rng(random_seed__, 0);
    // suppress unused var warning
    (void) base_rng__;
    static constexpr const char* function__ =
      "model_constant_single_ind_namespace::model_constant_single_ind";
    // suppress unused var warning
    (void) function__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      current_statement__ = 31;
      context__.validate_dims("data initialization", "n_obs", "int",
        std::vector<size_t>{});
      n_obs = std::numeric_limits<int>::min();
      current_statement__ = 31;
      n_obs = context__.vals_i("n_obs")[(1 - 1)];
      current_statement__ = 32;
      stan::math::validate_non_negative_index("y_obs", "n_obs", n_obs);
      current_statement__ = 33;
      context__.validate_dims("data initialization", "y_obs", "double",
        std::vector<size_t>{static_cast<size_t>(n_obs)});
      y_obs = std::vector<double>(n_obs,
                std::numeric_limits<double>::quiet_NaN());
      current_statement__ = 33;
      y_obs = context__.vals_r("y_obs");
      current_statement__ = 34;
      stan::math::validate_non_negative_index("obs_index", "n_obs", n_obs);
      current_statement__ = 35;
      context__.validate_dims("data initialization", "obs_index", "int",
        std::vector<size_t>{static_cast<size_t>(n_obs)});
      obs_index = std::vector<int>(n_obs, std::numeric_limits<int>::min());
      current_statement__ = 35;
      obs_index = context__.vals_i("obs_index");
      current_statement__ = 36;
      stan::math::validate_non_negative_index("time", "n_obs", n_obs);
      current_statement__ = 37;
      context__.validate_dims("data initialization", "time", "double",
        std::vector<size_t>{static_cast<size_t>(n_obs)});
      time = std::vector<double>(n_obs,
               std::numeric_limits<double>::quiet_NaN());
      current_statement__ = 37;
      time = context__.vals_r("time");
      current_statement__ = 38;
      context__.validate_dims("data initialization", "y_0_obs", "double",
        std::vector<size_t>{});
      y_0_obs = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 38;
      y_0_obs = context__.vals_r("y_0_obs")[(1 - 1)];
      current_statement__ = 39;
      stan::math::validate_non_negative_index("y_hat", "n_obs", n_obs);
      current_statement__ = 40;
      stan::math::validate_non_negative_index("Delta_hat", "n_obs", n_obs);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    num_params_r__ = 1 + 1 + 1;
  }
  inline std::string model_name() const final {
    return "model_constant_single_ind";
  }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.32.2",
             "stancflags = --allow-undefined"};
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI,
            stan::require_vector_like_t<VecR>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR>
  log_prob_impl(VecR& params_r__, VecI& params_i__, std::ostream*
                pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    static constexpr const char* function__ =
      "model_constant_single_ind_namespace::log_prob";
    // suppress unused var warning
    (void) function__;
    try {
      local_scalar_t__ ind_y_0 = DUMMY_VAR__;
      current_statement__ = 1;
      ind_y_0 = in__.template read_constrain_lb<local_scalar_t__,
                  jacobian__>(0, lp__);
      local_scalar_t__ ind_beta = DUMMY_VAR__;
      current_statement__ = 2;
      ind_beta = in__.template read_constrain_lb<local_scalar_t__,
                   jacobian__>(0, lp__);
      local_scalar_t__ global_error_sigma = DUMMY_VAR__;
      current_statement__ = 3;
      global_error_sigma = in__.template read_constrain_lb<local_scalar_t__,
                             jacobian__>(0, lp__);
      {
        current_statement__ = 17;
        stan::math::validate_non_negative_index("y_hat", "n_obs", n_obs);
        std::vector<local_scalar_t__> y_hat =
          std::vector<local_scalar_t__>(n_obs, DUMMY_VAR__);
        current_statement__ = 26;
        for (int i = 1; i <= n_obs; ++i) {
          current_statement__ = 21;
          if (stan::math::logical_eq(
                stan::model::rvalue(obs_index, "obs_index",
                  stan::model::index_uni(i)), 1)) {
            current_statement__ = 19;
            stan::model::assign(y_hat, ind_y_0, "assigning variable y_hat",
              stan::model::index_uni(i));
          }
          current_statement__ = 24;
          if (stan::math::logical_lt(i, n_obs)) {
            current_statement__ = 22;
            stan::model::assign(y_hat,
              size_step(
                stan::model::rvalue(y_hat, "y_hat", stan::model::index_uni(i)),
                ind_beta,
                (stan::model::rvalue(time, "time",
                   stan::model::index_uni((i + 1))) -
                stan::model::rvalue(time, "time", stan::model::index_uni(i))),
                pstream__), "assigning variable y_hat",
              stan::model::index_uni((i + 1)));
          }
        }
        current_statement__ = 27;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(y_obs, y_hat,
                         global_error_sigma));
        current_statement__ = 28;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(ind_y_0, y_0_obs,
                         global_error_sigma));
        current_statement__ = 29;
        lp_accum__.add(stan::math::lognormal_lpdf<propto__>(ind_beta, 0.1, 1));
        current_statement__ = 30;
        lp_accum__.add(stan::math::cauchy_lpdf<propto__>(global_error_sigma,
                         0.1, 1));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
  }
  template <typename RNG, typename VecR, typename VecI, typename VecVar,
            stan::require_vector_like_vt<std::is_floating_point,
            VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral,
            VecI>* = nullptr, stan::require_vector_vt<std::is_floating_point,
            VecVar>* = nullptr>
  inline void
  write_array_impl(RNG& base_rng__, VecR& params_r__, VecI& params_i__,
                   VecVar& vars__, const bool
                   emit_transformed_parameters__ = true, const bool
                   emit_generated_quantities__ = true, std::ostream*
                   pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    static constexpr bool propto__ = true;
    // suppress unused var warning
    (void) propto__;
    double lp__ = 0.0;
    // suppress unused var warning
    (void) lp__;
    int current_statement__ = 0;
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    constexpr bool jacobian__ = false;
    static constexpr const char* function__ =
      "model_constant_single_ind_namespace::write_array";
    // suppress unused var warning
    (void) function__;
    try {
      double ind_y_0 = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 1;
      ind_y_0 = in__.template read_constrain_lb<local_scalar_t__,
                  jacobian__>(0, lp__);
      double ind_beta = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 2;
      ind_beta = in__.template read_constrain_lb<local_scalar_t__,
                   jacobian__>(0, lp__);
      double global_error_sigma = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 3;
      global_error_sigma = in__.template read_constrain_lb<local_scalar_t__,
                             jacobian__>(0, lp__);
      out__.write(ind_y_0);
      out__.write(ind_beta);
      out__.write(global_error_sigma);
      if (stan::math::logical_negation(
            (stan::math::primitive_value(emit_transformed_parameters__) ||
            stan::math::primitive_value(emit_generated_quantities__)))) {
        return ;
      }
      if (stan::math::logical_negation(emit_generated_quantities__)) {
        return ;
      }
      std::vector<double> y_hat =
        std::vector<double>(n_obs, std::numeric_limits<double>::quiet_NaN());
      std::vector<double> Delta_hat =
        std::vector<double>(n_obs, std::numeric_limits<double>::quiet_NaN());
      current_statement__ = 16;
      for (int i = 1; i <= n_obs; ++i) {
        current_statement__ = 8;
        if (stan::math::logical_eq(
              stan::model::rvalue(obs_index, "obs_index",
                stan::model::index_uni(i)), 1)) {
          current_statement__ = 6;
          stan::model::assign(y_hat, ind_y_0, "assigning variable y_hat",
            stan::model::index_uni(i));
        }
        current_statement__ = 14;
        if (stan::math::logical_lt(i, n_obs)) {
          current_statement__ = 11;
          stan::model::assign(y_hat,
            size_step(
              stan::model::rvalue(y_hat, "y_hat", stan::model::index_uni(i)),
              ind_beta,
              (stan::model::rvalue(time, "time",
                 stan::model::index_uni((i + 1))) -
              stan::model::rvalue(time, "time", stan::model::index_uni(i))),
              pstream__), "assigning variable y_hat",
            stan::model::index_uni((i + 1)));
          current_statement__ = 12;
          stan::model::assign(Delta_hat,
            (stan::model::rvalue(y_hat, "y_hat",
               stan::model::index_uni((i + 1))) -
            stan::model::rvalue(y_hat, "y_hat", stan::model::index_uni(i))),
            "assigning variable Delta_hat", stan::model::index_uni(i));
        } else {
          current_statement__ = 9;
          stan::model::assign(Delta_hat, (DE(ind_beta, pstream__) *
            (stan::model::rvalue(time, "time", stan::model::index_uni(i)) -
            stan::model::rvalue(time, "time", stan::model::index_uni((i - 1))))),
            "assigning variable Delta_hat", stan::model::index_uni(i));
        }
      }
      out__.write(y_hat);
      out__.write(Delta_hat);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, typename VecI,
            stan::require_vector_t<VecVar>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void
  unconstrain_array_impl(const VecVar& params_r__, const VecI& params_i__,
                         VecVar& vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ ind_y_0 = DUMMY_VAR__;
      current_statement__ = 1;
      ind_y_0 = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, ind_y_0);
      local_scalar_t__ ind_beta = DUMMY_VAR__;
      current_statement__ = 2;
      ind_beta = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, ind_beta);
      local_scalar_t__ global_error_sigma = DUMMY_VAR__;
      current_statement__ = 3;
      global_error_sigma = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, global_error_sigma);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, stan::require_vector_t<VecVar>* = nullptr>
  inline void
  transform_inits_impl(const stan::io::var_context& context__, VecVar&
                       vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      current_statement__ = 1;
      context__.validate_dims("parameter initialization", "ind_y_0",
        "double", std::vector<size_t>{});
      current_statement__ = 2;
      context__.validate_dims("parameter initialization", "ind_beta",
        "double", std::vector<size_t>{});
      current_statement__ = 3;
      context__.validate_dims("parameter initialization",
        "global_error_sigma", "double", std::vector<size_t>{});
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ ind_y_0 = DUMMY_VAR__;
      current_statement__ = 1;
      ind_y_0 = context__.vals_r("ind_y_0")[(1 - 1)];
      out__.write_free_lb(0, ind_y_0);
      local_scalar_t__ ind_beta = DUMMY_VAR__;
      current_statement__ = 2;
      ind_beta = context__.vals_r("ind_beta")[(1 - 1)];
      out__.write_free_lb(0, ind_beta);
      local_scalar_t__ global_error_sigma = DUMMY_VAR__;
      current_statement__ = 3;
      global_error_sigma = context__.vals_r("global_error_sigma")[(1 - 1)];
      out__.write_free_lb(0, global_error_sigma);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  inline void
  get_param_names(std::vector<std::string>& names__, const bool
                  emit_transformed_parameters__ = true, const bool
                  emit_generated_quantities__ = true) const {
    names__ = std::vector<std::string>{"ind_y_0", "ind_beta",
                "global_error_sigma"};
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {
      std::vector<std::string> temp{"y_hat", "Delta_hat"};
      names__.reserve(names__.size() + temp.size());
      names__.insert(names__.end(), temp.begin(), temp.end());
    }
  }
  inline void
  get_dims(std::vector<std::vector<size_t>>& dimss__, const bool
           emit_transformed_parameters__ = true, const bool
           emit_generated_quantities__ = true) const {
    dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{},
                std::vector<size_t>{}, std::vector<size_t>{}};
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {
      std::vector<std::vector<size_t>>
        temp{std::vector<size_t>{static_cast<size_t>(n_obs)},
             std::vector<size_t>{static_cast<size_t>(n_obs)}};
      dimss__.reserve(dimss__.size() + temp.size());
      dimss__.insert(dimss__.end(), temp.begin(), temp.end());
    }
  }
  inline void
  constrained_param_names(std::vector<std::string>& param_names__, bool
                          emit_transformed_parameters__ = true, bool
                          emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "ind_y_0");
    param_names__.emplace_back(std::string() + "ind_beta");
    param_names__.emplace_back(std::string() + "global_error_sigma");
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= n_obs; ++sym1__) {
        param_names__.emplace_back(std::string() + "y_hat" + '.' +
          std::to_string(sym1__));
      }
      for (int sym1__ = 1; sym1__ <= n_obs; ++sym1__) {
        param_names__.emplace_back(std::string() + "Delta_hat" + '.' +
          std::to_string(sym1__));
      }
    }
  }
  inline void
  unconstrained_param_names(std::vector<std::string>& param_names__, bool
                            emit_transformed_parameters__ = true, bool
                            emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "ind_y_0");
    param_names__.emplace_back(std::string() + "ind_beta");
    param_names__.emplace_back(std::string() + "global_error_sigma");
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= n_obs; ++sym1__) {
        param_names__.emplace_back(std::string() + "y_hat" + '.' +
          std::to_string(sym1__));
      }
      for (int sym1__ = 1; sym1__ <= n_obs; ++sym1__) {
        param_names__.emplace_back(std::string() + "Delta_hat" + '.' +
          std::to_string(sym1__));
      }
    }
  }
  inline std::string get_constrained_sizedtypes() const {
    return std::string("[{\"name\":\"ind_y_0\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"ind_beta\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"global_error_sigma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"y_hat\",\"type\":{\"name\":\"array\",\"length\":" + std::to_string(n_obs) + ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"},{\"name\":\"Delta_hat\",\"type\":{\"name\":\"array\",\"length\":" + std::to_string(n_obs) + ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"}]");
  }
  inline std::string get_unconstrained_sizedtypes() const {
    return std::string("[{\"name\":\"ind_y_0\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"ind_beta\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"global_error_sigma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"y_hat\",\"type\":{\"name\":\"array\",\"length\":" + std::to_string(n_obs) + ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"},{\"name\":\"Delta_hat\",\"type\":{\"name\":\"array\",\"length\":" + std::to_string(n_obs) + ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"}]");
  }
  // Begin method overload boilerplate
  template <typename RNG> inline void
  write_array(RNG& base_rng, Eigen::Matrix<double,-1,1>& params_r,
              Eigen::Matrix<double,-1,1>& vars, const bool
              emit_transformed_parameters = true, const bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + 1);
    const size_t num_transformed = emit_transformed_parameters * (0);
    const size_t num_gen_quantities = emit_generated_quantities * ((n_obs +
      n_obs));
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    std::vector<int> params_i;
    vars = Eigen::Matrix<double,-1,1>::Constant(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <typename RNG> inline void
  write_array(RNG& base_rng, std::vector<double>& params_r, std::vector<int>&
              params_i, std::vector<double>& vars, bool
              emit_transformed_parameters = true, bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + 1);
    const size_t num_transformed = emit_transformed_parameters * (0);
    const size_t num_gen_quantities = emit_generated_quantities * ((n_obs +
      n_obs));
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    vars = std::vector<double>(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(Eigen::Matrix<T_,-1,1>& params_r, std::ostream* pstream = nullptr) const {
    Eigen::Matrix<int,-1,1> params_i;
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(std::vector<T_>& params_r, std::vector<int>& params_i,
           std::ostream* pstream = nullptr) const {
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  inline void
  transform_inits(const stan::io::var_context& context,
                  Eigen::Matrix<double,-1,1>& params_r, std::ostream*
                  pstream = nullptr) const final {
    std::vector<double> params_r_vec(params_r.size());
    std::vector<int> params_i;
    transform_inits(context, params_i, params_r_vec, pstream);
    params_r = Eigen::Map<Eigen::Matrix<double,-1,1>>(params_r_vec.data(),
                 params_r_vec.size());
  }
  inline void
  transform_inits(const stan::io::var_context& context, std::vector<int>&
                  params_i, std::vector<double>& vars, std::ostream*
                  pstream__ = nullptr) const {
    vars.resize(num_params_r__);
    transform_inits_impl(context, vars, pstream__);
  }
  inline void
  unconstrain_array(const std::vector<double>& params_constrained,
                    std::vector<double>& params_unconstrained, std::ostream*
                    pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = std::vector<double>(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
  inline void
  unconstrain_array(const Eigen::Matrix<double,-1,1>& params_constrained,
                    Eigen::Matrix<double,-1,1>& params_unconstrained,
                    std::ostream* pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = Eigen::Matrix<double,-1,1>::Constant(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
};
}
using stan_model = model_constant_single_ind_namespace::model_constant_single_ind;
#ifndef USING_R
// Boilerplate
stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_constant_single_ind_namespace::profiles__;
}
#endif
#endif
