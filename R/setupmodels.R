# models_step <- list(
#   c(function(df) { lm("MDS_UPDRS_I~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education", df)},
#     function(df) { lm("MDS_UPDRS_II~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I",df)},
#     function(df) { lm("MDS_UPDRS_III_other~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II",df)},
#     function(df) { lm("MSEADLG~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)}),
#   c(function(df) { lm("MDS_UPDRS_I~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education", df)},
#     function(df) { lm("MDS_UPDRS_II~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I",df)},
#     function(df) { lm("MDS_UPDRS_III_other~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II",df)},
#     function(df) { lm("MSEADLG~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)},
#     function(df) { lm("MoCA~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)}),
#   c(function(df) { lm("MDS_UPDRS_I~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education", df)},
#     function(df) { lm("MDS_UPDRS_II~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I",df)},
#     function(df) { lm("MDS_UPDRS_III_other~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II",df)},
#     function(df) { lm("MSEADLG~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)}),
#   c(function(df) { lm("MDS_UPDRS_I_year2~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education",df)},
#     function(df) { lm("MDS_UPDRS_II_year2~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2",df)},
#     function(df) { lm("MDS_UPDRS_III_other_year2~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2",df)},
#     function(df) { lm("MSEADLG_year2~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2+MDS_UPDRS_III_other_year2",df)},
#     function(df) { lm("MoCA_year2~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2+MDS_UPDRS_III_other_year2",df)},
#     function(df) { lm("MDS_UPDRS_I_year3~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education",df)},
#     function(df) { lm("MDS_UPDRS_II_year3~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3",df)},
#     function(df) { lm("MDS_UPDRS_III_other_year3~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3",df)},
#     function(df) { lm("MSEADLG_year3~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3+MDS_UPDRS_III_other_year3",df)},
#     function(df) { lm("MoCA_year3~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3+MDS_UPDRS_III_other_year3",df)},
#     function(df) { lm("MDS_UPDRS_I_year4~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education",df)},
#     function(df) { lm("MDS_UPDRS_II_year4~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4",df)},
#     function(df) { lm("MDS_UPDRS_III_other_year4~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4",df)},
#     function(df) { lm("MSEADLG_year4~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4+MDS_UPDRS_III_other_year4",df)},
#     function(df) { lm("MoCA_year4~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4+MDS_UPDRS_III_other_year4",df)}
#   )
# )

## Intermediate models

models_step <- list(
  c(function(df) { lm("MDS_UPDRS_I~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education", df)},
    function(df) { lm("MDS_UPDRS_II~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I",df)},
    function(df) { lm("MDS_UPDRS_III_other~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II",df)},
    function(df) { lm("MSEADLG~treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)}),
  c(function(df) { lm("MDS_UPDRS_I~treatment+treatment_lag1+Duration+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education", df)},
    function(df) { lm("MDS_UPDRS_II~treatment+treatment_lag1+Duration+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I",df)},
    function(df) { lm("MDS_UPDRS_III_other~treatment+treatment_lag1+Duration+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II",df)},
    function(df) { lm("MSEADLG~treatment+treatment_lag1+Duration+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)},
    function(df) { lm("MoCA~treatment+treatment_lag1+Duration+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)}),
  c(function(df) { lm("MDS_UPDRS_I~treatment+treatment_lag1+Duration+treatment_lag1+treatment_lag2+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education", df)},
    function(df) { lm("MDS_UPDRS_II~treatment+treatment_lag1+Duration+treatment_lag1+treatment_lag2+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I",df)},
    function(df) { lm("MDS_UPDRS_III_other~treatment+treatment_lag1+Duration+treatment_lag1+treatment_lag2+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II",df)},
    function(df) { lm("MSEADLG~treatment+treatment_lag1+Duration+treatment_lag1+treatment_lag2+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+Age+Sex+Education+MDS_UPDRS_I+MDS_UPDRS_II+MDS_UPDRS_III_other",df)}),
  c(function(df) { lm("MDS_UPDRS_I_year2~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education",df)},
    function(df) { lm("MDS_UPDRS_II_year2~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2",df)},
    function(df) { lm("MDS_UPDRS_III_other_year2~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2",df)},
    function(df) { lm("MDS_UPDRS_III_off_year2~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2",df)},
    function(df) { lm("MSEADLG_year2~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2+MDS_UPDRS_III_other_year2",df)},
    function(df) { lm("MoCA_year2~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year2+MDS_UPDRS_II_year2+MDS_UPDRS_III_other_year2",df)},
    function(df) { lm("MDS_UPDRS_I_year3~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education",df)},
    function(df) { lm("MDS_UPDRS_II_year3~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3",df)},
    function(df) { lm("MDS_UPDRS_III_other_year3~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3",df)},
    function(df) { lm("MDS_UPDRS_III_off_year3~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3",df)},
    function(df) { lm("MSEADLG_year3~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3+MDS_UPDRS_III_other_year3",df)},
    function(df) { lm("MoCA_year3~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year3+MDS_UPDRS_II_year3+MDS_UPDRS_III_other_year3",df)},
    function(df) { lm("MDS_UPDRS_I_year4~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education",df)},
    function(df) { lm("MDS_UPDRS_II_year4~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4",df)},
    function(df) { lm("MDS_UPDRS_III_other_year4~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4",df)},
    function(df) { lm("MDS_UPDRS_III_off_year4~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4",df)},
    function(df) { lm("MSEADLG_year4~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4+MDS_UPDRS_III_other_year4",df)},
    function(df) { lm("MoCA_year4~treatment+treatment_lag1+treatment_lag2+treatment_lag3+treatment+Duration+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag2+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+Age+Sex+Education+MDS_UPDRS_I_year4+MDS_UPDRS_II_year4+MDS_UPDRS_III_other_year4",df)}
  )
)

models_ipw_prior <- list(
  list(function(df) {glm(formula("treatment~censored+1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("treatment~censored+treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("treatment~censored+treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("treatment~censored+treatment_lag1"), data=df, family=binomial(link="logit")) }),
  list(function(df) {glm(formula("censored~1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) })
) %>% purrr::transpose()

models_ipw <- list(
  list(function(df) {glm(formula("treatment~censored+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("treatment~censored+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("treatment~censored+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("treatment~censored+treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) }),
  list(function(df) {glm(formula("censored~MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) })
       ) %>% purrr::transpose()


models_ipw_cens_prior <- list(
  list(function(df) {glm(formula("censored~1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) })
) %>% purrr::transpose()

models_ipw_cens <- list(
  list(function(df) {glm(formula("censored~MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+MoCA_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) },
       function(df) {glm(formula("censored~treatment_lag1+MDS_UPDRS_I_lag1+MDS_UPDRS_II_lag1+MDS_UPDRS_III_other_lag1+MSEADLG_lag1+
                                 Age+Education+Sex+Duration"), data=df, family=binomial(link="logit")) })
       ) %>% purrr::transpose()


## Marginal Structural Models
generate_msms <- function(vars, vars_extra=NULL,ipw_interaction=FALSE) {
  
  vars_first <- vars
  if (!is.null(vars_extra)) {
    vars_first <- c(vars,vars_extra)
  }
 
  msms <- expand.grid(var=vars_first,
                      year=c("_year2","_year3","_year4"),
                      model=c("~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)",
                              "~treatment+treatment_lag1+treatment_lag2+treatment_lag3")) %>%
    mutate(sf=pmap_chr(list(var,year,model),~paste0(..1,..2,..3))) %>%
    .$sf %>%
    map(formula)
  
  msms_adj <- expand.grid(var=vars,
                          year=c("_year2","_year3","_year4"),
                          model=c("~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+",
                                  "~treatment+treatment_lag1+treatment_lag2+treatment_lag3+")) %>%
    mutate(sf=pmap_chr(list(var,year,model),~paste0(..1,..2,..3,..1,"_lag4"))) %>%
    .$sf %>%
    map(formula)
  
  
  msms_adj_full <- expand.grid(var=vars,
                               year=c("_year2","_year3","_year4"),
                               model=c("~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+")) %>%
    mutate(sf=pmap_chr(list(var,year,model),~paste0(..1,..2,..3,"MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4+MSEADLG_lag4+MoCA_lag4+
                                   Age+Education+Sex+Duration"))) %>%
    .$sf %>%
    map(formula)
  
  msms_adj_ledd <- expand.grid(var=vars,
                               year=c("_year2","_year3","_year4"),
                               model=c("~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)+")) %>%
    mutate(sf=pmap_chr(list(var,year,model),~paste0(..1,..2,..3,..1,"_lag4","+LEDD",..2))) %>%
    .$sf %>%
    map(formula)
  
  
  
  msms_all <- c(msms, msms_adj,msms_adj_full, msms_adj_ledd)
  names(msms_all) <- c(rep("msms",length(msms)),
                       rep("msms_adj",length(msms_adj)),
                       rep("msms_adj_full",length(msms_adj_full)),
                       rep("msms_adj_ledd",length(msms_adj_ledd)))
  
  if (ipw_interaction) {
    msms_interaction <- expand.grid(var=c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MDS_UPDRS_III_off"),
                                    year=c("_year2","_year3","_year4"),
                                    model=c("~I((treatment+treatment_lag1+treatment_lag2+treatment_lag3)/2)*(Age+Sex+Duration+MDS_UPDRS_I_lag4+MDS_UPDRS_II_lag4+MDS_UPDRS_III_other_lag4)")) %>%
      mutate(sf=pmap_chr(list(var,year,model),~paste0(..1,..2,..3))) %>%
      .$sf %>%
      map(formula)
    
    
    msms_all <- c(msms, msms_adj, msms_adj_full, msms_adj_ledd, msms_interaction)
    names(msms_all) <- c(rep("msms",length(msms)),
                         rep("msms_adj",length(msms_adj)),
                         rep("msms_adj_full",length(msms_adj_full)),
                         rep("msms_adj_ledd",length(msms_adj_ledd)),
                         rep("msms_interaction",length(msms_interaction))
    )
  }
  
  return(msms_all)
}

vars_1 <- c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MDS_UPDRS_III_off","MoCA","MSEADLG")
vars_2 <- c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MDS_UPDRS_III_off","MoCA","MSEADLG","QUIP")
vars_extra <- c("QUIP8","QUIPany","NP4WDYSK","NP4OFF")

msms_all <- generate_msms(vars_1)
msms_ipw <- generate_msms(vars_2,vars_extra,ipw_interaction=TRUE) # IPW: Extra: interactions, different outcomes for different models

# Starters: different length of treatment specification: c("~I((treatment_lag1+treatment_lag2+treatment_lag3)*2/3)+","~treatment_lag1+treatment_lag2+treatment_lag3+")
# Potential problem: will crash because of treatment being constant

# Helper functions

predict_mc.lm <- function(object, new_data) {

  out <- predict(object, new_data)

  if(any(is.na(object$coefficients))) {
    #print(names(object$coefficients)[which(is.na(object$coefficients))])
    intraining <- object$model[1,which(is.na(object$coefficients)),drop=FALSE]
    allowed <- apply(new_data[,names(intraining),drop=FALSE]==intraining[rep(1,nrow(new_data)),,drop=FALSE],1,all)
    out[!allowed] <- NA
  }
  
  
  pred <- tibble( rnorm(nrow(new_data))*sd(object$residuals)+out)
  names(pred) <- object$terms %>%  all.vars %>% .[[1]]
  
  
  pred
}

predict_dens.glm <- function(object,new_data) {
  treat_name <- object$terms %>%  all.vars %>% .[[1]]
  new_data[[treat_name]]*predict(object, new_data,type="response") +
    (1-new_data[[treat_name]])*(1-predict(object, new_data,type="response")) %>%
    data.frame() -> pred
  names(pred) <- treat_name
  pred
}



# Model execution

# run_once <- function(df_v,df_c) {
# 
#   out_naive <- msm_naive(df_v, msms_all, models_ipw_cens, models_ipw_cens_prior, data_constant = df_c)
#   out_cens <- msm_ipw(df_v, msms_all, models_ipw_cens, models_ipw_cens_prior, data_constant = df_c)
#   out_ipw <- msm_ipw(df_v, msms_all, models_ipw, models_ipw_prior, data_constant = df_c)
#   out_gform <- par_g_formula(df_v,msm=msms_all,models=models_step,data_constant=df_c, mc_samples=50, save=TRUE)
# 
#   bind_rows(out_naive %>% map_dfr(tidy, quick=TRUE) %>% mutate(Model="Naive"),
#             out_cens %>% map_dfr(tidy, quick=TRUE) %>% mutate(Model="Censored"),
#             out_ipw %>% map_dfr(tidy, quick=TRUE) %>% mutate(Model="IPW"),
#             out_gform %>% map_dfr(tidy, quick=TRUE) %>% mutate(Model="gformula")
#   )
# }

run_once <- function(df_v,df_c,df_l, msms_all, msms_ipw, compact=TRUE) {
  out_naive <- tibble(msm=msms_ipw,
                      modelname=names(msms_ipw),
                      outcome=map_chr(msm,~all.vars(terms(.x))[[1]]),
                      form=map_chr(msm,~Reduce(paste,deparse(formula(.x)))) %>% str_replace(".*~ ",""),
                      fitted=msm_naive(df_v, msms_ipw, data_constant = df_c, data_last=df_l)
  )

  out_ipw <- tibble(msm=msms_ipw) %>%
    mutate(outcome=map_chr(msm,~all.vars(terms(.x))[[1]])) %>%
    mutate(modelname=names(msms_ipw)) %>%
    group_by(outcome) %>% nest %>%
    mutate(fitted=map(data, function(.x) {msm_ipw(df_v, .x$msm, models_ipw, models_ipw_prior, data_constant = df_c,data_last=df_l, censor_outcome = TRUE)})) %>%
    mutate(fitted=map(fitted,~.[1:length(.)])) %>% unnest(cols = c(data, fitted)) %>% ungroup %>%
    mutate(outcome=map_chr(msm,~all.vars(terms(.x))[[1]]),
           form=map_chr(msm,~Reduce(paste,deparse(formula(.x)))) %>% str_replace(".*~ ",""))


  out_cens <- tibble(msm=msms_ipw) %>%
    mutate(outcome=map_chr(msm,~all.vars(terms(.x))[[1]])) %>%
    mutate(modelname=names(msms_ipw)) %>%
    group_by(outcome) %>% nest %>%
    mutate(fitted=map(data, function(.x) {msm_ipw(df_v, .x$msm, models_ipw_cens, models_ipw_cens_prior, data_constant = df_c,data_last=df_l, censor_outcome = TRUE)})) %>%
    mutate(fitted=map(fitted,~.[1:length(.)])) %>% unnest(cols = c(data, fitted)) %>% ungroup %>%
    mutate(outcome=map_chr(msm,~all.vars(terms(.x))[[1]]),
           form=map_chr(msm,~Reduce(paste,deparse(formula(.x)))) %>% str_replace(".*~ ",""))

  treats <- list(
    data.frame(time=1:4,treatment=c(0,0,0,0)),
    data.frame(time=1:4,treatment=c(0,0,0,1)),
    data.frame(time=1:4,treatment=c(0,0,1,1)),
    data.frame(time=1:4,treatment=c(0,1,1,1)),
    data.frame(time=1:4,treatment=c(1,1,1,1))
  )

  out_gform <- par_g_formula_fast(df_v,msm=msms_all,models=models_step,data_constant=df_c,data_last=df_l, mc_samples=100, save=TRUE, treatment_assignment = treats)
  
  out_gform <- tibble(msm=msms_all,
                      modelname=names(msms_all),
                      outcome=map_chr(msm,~all.vars(terms(.x))[[1]]),
                      form=map_chr(msm,~Reduce(paste,deparse(formula(.x)))) %>% str_replace(".*~ ",""),
                      fitted=out_gform
  )

  out <- bind_rows(out_naive %>% mutate(method="Naive"),
                   out_cens %>% mutate(method="Censoring"),
                   out_ipw %>% mutate(method="IPW"),
                   out_gform %>% mutate(method="gformula"))

  if (compact) {
        out <- out %>% select(-msm) %>%
      mutate(ti=map(fitted,tidy,quick=TRUE)) %>%
      mutate(obs=map_int(fitted,function(x){nrow(x$model)})) %>% 
      select(-fitted) %>% unnest(cols = c(ti))
  }
  out
}


#' Bootstrap and run models
run_bootstrap <- function(df_varying,df_constant,df_l,msms_all,msms_ipw, repeats=1000) {
  future_map(1:repeats, function(x,msms_all,msms_ipw) {
    sp <- bootstrap_sample2(unique(df_varying$id),df_varying,df_constant,df_l)
    run_once(sp$df_measurements,sp$df_outcomes,sp$df_outcomes2,msms_all,msms_ipw)
  },.progress = TRUE,msms_all=msms_all,msms_ipw=msms_ipw) %>% bind_rows(.id="rep")
}
