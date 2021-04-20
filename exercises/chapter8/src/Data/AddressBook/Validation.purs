module Data.AddressBook.Validation where

import Prelude
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType, address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Common (null)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Array (filter)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType

derive instance eqField :: Eq Field

data ValidationSubject = Field Field
                       | Form

instance showField :: Show Field where
  show FirstNameField = "First Name"
  show LastNameField = "Last Name"
  show StreetField = "Street"
  show CityField = "City"
  show StateField = "State"
  show (PhoneField _) = "Phone Number"

data ValidationError = ValidationError String ValidationSubject

type Errors = Array ValidationError

isFormError :: ValidationError -> Boolean
isFormError (ValidationError _ Form) = true
isFormError _ = false

isFieldError :: Field -> ValidationError -> Boolean
isFieldError field (ValidationError _ (Field errorField)) = field == errorField
isFieldError _ _ = false

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" <> show field <> "' cannot be empty") (Field field)]
nonEmpty _     _  = pure unit

filterPresentPhoneNumbers :: Array PhoneNumber -> Array PhoneNumber
filterPresentPhoneNumbers numbers = filter (\(PhoneNumber { number }) -> not $ null number) numbers

validateAnyPhoneNumber :: Array PhoneNumber -> V Errors Unit
validateAnyPhoneNumber [] = invalid [ValidationError "There must be at least one phone number" Form]
validateAnyPhoneNumber _  = pure unit

lengthIs :: Field -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len =
    invalid [ValidationError ("Field '" <> show field <> "' must have length " <> show len) (Field field)]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: Field -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     =
  invalid [ValidationError ("Field '" <> show field <> "' did not match the required format") (Field field)]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o."type") phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (validateAnyPhoneNumber (filterPresentPhoneNumbers o.phones) *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
