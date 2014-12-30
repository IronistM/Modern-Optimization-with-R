# four EDA types:
# adapted from (Gonzalez-Fernandez and Soto, 2012)
UMDA=CEDA(copula="indep",margin="norm"); UMDA@name="UMDA"
GCEDA=CEDA(copula="normal",margin="norm"); GCEDA@name="GCEDA"
CVEDA=VEDA(vine="CVine",indepTestSigLevel=0.01,
           copulas = c("normal"),margin = "norm")
CVEDA@name="CVEDA"
DVEDA=VEDA(vine="DVine",indepTestSigLevel=0.01,
           copulas = c("normal"),margin = "norm")
DVEDA@name="DVEDA"

