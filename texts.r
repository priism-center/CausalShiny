
tag1 <- tagList(
        tags$h5("Estimand: quantity to estimate"),
        tags$p("The three options are Average Treatment Effect (ATE), Average Treatment Effect 
               on the Control (ATC), and Average Treatment Effect on the Treated (ATT). The 
               three estimands are defined under potential outcomes framework. For an 
               individual, the treatment effect is defined as the difference between the actual 
               outcome and its counterfactual. Counterfactual is the outcome had the individual 
               (not) received treatment or the alternative of the factual outcome."),
        tags$img(src = 'potential_outcome.png', alt = "Not available",
                 height = 400),
        tags$h5("ATE: "),
        tags$p("Use ATE when the main target of inference is everyone in the sample"),
        tags$h5("ATC: "), 
        tags$p("Use ATC when the main target of inference are the controls in the sample"),
        tags$h5("ATT: "),
        tags$p("Use ATC when the main target of inference are the treated in the sample")
        )
text11 <- 

text2 <- "Propensity score measures the probability of receiving treatment"

text3 <- "Targeted Maximum likelihood Estimation: Mark Van Der Laan, Daniel Rubin"