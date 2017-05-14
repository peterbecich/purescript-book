module Data.AddressBook.Validation where

import Prelude
import Data.Generic
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

-- should probably be in AddressBook module instead
data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType
           | PhoneNumbers

derive instance genericField :: Generic Field

instance showField :: Show Field where
  --show = gShow
  show FirstNameField = "first name"
  show LastNameField = "last name"
  show StreetField = "street"
  show CityField = "city"
  show StateField = "state"
  show (PhoneField phoneType) = (show phoneType) <> " phone"
  show PhoneNumbers = "phone numbers"

instance eqField :: Eq Field where
  eq = gEq
  

data ValidationError = ValidationError String Field

fieldFilterPredicate :: Field -> ValidationError -> Boolean
fieldFilterPredicate field (ValidationError _ field')
  | field == field' = true
  | otherwise = false

derive instance genericValidationError :: Generic ValidationError

instance showValidationError :: Show ValidationError where
  -- show = gShow
  show (ValidationError errString _) = errString


type Errors = Array ValidationError

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" <> show field <> "' cannot be empty") field]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. Field -> Array a -> V Errors Unit
arrayNonEmpty field [] =
  invalid [ValidationError ("Field '" <> show field <> "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len =
  invalid [ValidationError ("Field '" <> show field <> "' must have length " <> show len) field]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: Field -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     =
  invalid [ValidationError ("Field '" <> show field <> "' did not match the required format") field]

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
         <*> (arrayNonEmpty PhoneNumbers o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p

