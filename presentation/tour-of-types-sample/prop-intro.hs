type Form = ()
type ValidForm = ()
type Result = ()
type Error = ()

invalidFormError :: Error

validateForm :: Form -> Maybe ValidForm

submitForm :: ValidForm -> Result

processForm :: Form -> Either Error Result
processForm form =
    case validateForm of
        Just validForm -> Right (submitForm validForm)
        Nothing        -> Left invalidFormError
