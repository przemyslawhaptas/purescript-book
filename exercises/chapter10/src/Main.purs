module Main where

import Prelude

import Effect (Effect)
import Effect.Alert (alert)
import Effect.Console (log)
import Effect.Storage (setItem, getItem)
import Control.Monad.Except (runExcept)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType(..), phoneNumber, address, person, examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Foreign (ForeignError, readNullOrUndefined, readString, renderForeignError, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.Index (index)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactThis, createLeafElement, getProps, getState, setState, component)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import React.SyntheticEvent (SyntheticInputEvent)

type AppState =
  { person :: Person
  , errors :: Errors
  }

type AppProps =
  { initialState :: AppState
  }

initialState :: Maybe FormData -> AppState
initialState Nothing = { person: examplePerson, errors: [] }
initialState (Just (FormData
  { firstName
  , lastName
  , street
  , city
  , state
  , homePhone
  , cellPhone
  }
)) =
  { person: (person firstName lastName (address street city state) phones)
  , errors: []
  } where
    phones = [ phoneNumber HomePhone homePhone, phoneNumber CellPhone cellPhone ]

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  , street     :: String
  , city       :: String
  , state      :: String
  , homePhone  :: String
  , cellPhone  :: String
  }

derive instance genericFormData :: Generic FormData _

instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

toFormData :: Partial => Person -> FormData
toFormData (Person p@{ homeAddress: Address a
                     , phones: [ PhoneNumber pn1
                               , PhoneNumber pn2
                               ]
                     }) =
  FormData { firstName  : p.firstName
           , lastName   : p.lastName
           , street     : a.street
           , city       : a.city
           , state      : a.state
           , homePhone  : pn1.number
           , cellPhone  : pn2.number
           }

loadSavedData :: Effect (Maybe FormData)
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: Either (NonEmptyList ForeignError) (Maybe FormData)
    savedData = runExcept do
      jsonOrNull <- traverse readString =<< readNullOrUndefined item
      traverse decodeJSON jsonOrNull

  case savedData of
    Left err -> do
      alert $ "Unable to read saved form data: " <> foldMap (("\n" <> _) <<< renderForeignError) err
      pure Nothing
    Right mdata -> pure mdata

validateAndSaveEntry :: Person -> Effect Unit
validateAndSaveEntry person = do
  log "Running validators"
  case validatePerson' person of
    Left errs -> alert $ "There are " <> show (length errs) <> " validation errors."
    Right result -> do
      setItem "person" $ encodeJSON $ unsafePartial toFormData result
      alert "Saved"

valueOf :: SyntheticInputEvent -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- index (unsafeToForeign e) "target"
  value <- index target "value"
  readString value

updateAppState
  :: ReactThis AppProps AppState
  -> (String -> Person)
  -> SyntheticInputEvent
  -> Effect Unit
updateAppState ctx update e = do
  for_ (valueOf e) \s -> do
    let newPerson = update s

    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> setState ctx ({ person: newPerson, errors: errors })
      Right _ -> setState ctx ({ person: newPerson, errors: [] })


addressBook :: ReactClass AppProps
addressBook = component "AddressBook" \ctx -> do
  props <- getProps ctx
  pure $
    { state: props.initialState
    , render: render ctx
    }
  where

  render ctx = do
    { person: Person person@{ homeAddress: Address address }, errors } <- getState ctx

    let renderValidationError err = D.li' [ D.text err ]

        renderValidationErrors [] = []
        renderValidationErrors xs =
          [ D.div [ P.className "alert alert-danger" ]
                  [ D.ul' (map renderValidationError xs) ]
          ]

        formField name hint value update =
          D.div [ P.className "form-group" ]
                [ D.label [ P.className "col-sm-2 control-label" ]
                          [ D.text name ]
                , D.div [ P.className "col-sm-3" ]
                        [ D.input [ P._type "text"
                                  , P.className "form-control"
                                  , P.placeholder hint
                                  , P.value value
                                  , P.onChange (updateAppState ctx update)
                                  ]
                        ]
                ]

        renderPhoneNumber (PhoneNumber phone) index =
          formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
            Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }

        updateFirstName s = Person $ person { firstName = s }
        updateLastName  s = Person $ person { lastName  = s }

        updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
        updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
        updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

        updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }

    pure $
      D.div [ P.className "container" ]
            [ D.div [ P.className "row" ]
                    (renderValidationErrors errors)
            , D.div [ P.className "row" ]
                    [ D.form [ P.className "form-horizontal" ] $
                             [ D.h3' [ D.text "Basic Information" ]

                             , formField "First Name" "First Name" person.firstName updateFirstName
                             , formField "Last Name"  "Last Name"  person.lastName  updateLastName

                             , D.h3' [ D.text "Address" ]

                             , formField "Street" "Street" address.street updateStreet
                             , formField "City"   "City"   address.city   updateCity
                             , formField "State"  "State"  address.state  updateState

                             , D.h3' [ D.text "Contact Information" ]
                             ]
                             <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                             <> [ D.div [ P.className "form-group" ]
                                   [ D.label [ P.className "col-sm-3 col-sm-offset-2" ]
                                             [ D.button [ P.className "btn btn-primary"
                                                        , P.onClick \_ -> validateAndSaveEntry (Person person)
                                                        ]
                                                        [ D.text "Save" ]
                                             ]
                                   ]
                                ]
                    ]
            ]


main :: Effect Unit
main = void do
  log "Loading data from local storage"
  formData <- loadSavedData

  log "Rendering address book component"
  let component = D.div [] [ createLeafElement addressBook { initialState: initialState formData } ]
  doc <- window >>= document
  ctr <- getElementById "main" (toNonElementParentNode (toDocument doc))
  render component (unsafePartial fromJust ctr)
