module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.Except (runExcept)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (fromJust, fromMaybe)
import Data.List.NonEmpty (NonEmptyList)
import Foreign (ForeignError, readString, unsafeToForeign)
import Foreign.Index (index)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactThis, createLeafElement, getState, component, writeState)
import React.SyntheticEvent (SyntheticInputEvent)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

type AppState =
  { person :: Person
  , errors :: Errors
  }

type AppProps = {}

initialState :: AppState
initialState =
  { person: examplePerson
  , errors: []
  }

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
updateAppState ctx update e =
  for_ (valueOf e) \s -> do
    let newPerson = update s

    logShow "Running validators"
    case validatePerson' newPerson of
      Left errors -> writeState ctx ({ person: newPerson, errors: errors })
      Right _ -> writeState ctx ({ person: newPerson, errors: [] })

addressBook :: ReactClass AppProps
addressBook = component "AddressBook" componentBody
  where
  componentBody this =
    pure { state: initialState
         , render: render this
         }
    where
    render ctx = do
      { person: Person person@{ homeAddress: Address address }, errors } <- getState ctx

      let renderValidationError err = D.li [ P.className "alert alert-danger" ] [ D.text err ]

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
                      (map renderValidationError errors)
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
                      ]
              ]

main :: Effect Unit
main = void do
  logShow "Rendering address book component"
  let component = D.div [] [ createLeafElement addressBook {} ]
  doc <- window >>= document
  ctr <- getElementById "main" (toNonElementParentNode (toDocument doc))
  render component (unsafePartial fromJust ctr)
