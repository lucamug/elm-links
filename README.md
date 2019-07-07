# elm-resources

[![Netlify Status](https://api.netlify.com/api/v1/badges/aec6dcc8-3e63-47fb-8ec2-a675e0e9492e/deploy-status)](https://app.netlify.com/sites/elm-resources/deploys)

## A collection of Elm Resources

## [https://elm-resources.guupa.com/](https://elm-resources.guupa.com/)

[![Screenshot](https://elm-resources.guupa.com/img/elm-resources.png)](https://elm-resources.guupa.com/)



Feel free to add resources sending Pull Requests or opening Issues.

## How to add a link

Edit the file `src/data/Links.elm` [clicking here](https://github.com/lucamug/elm-resources/edit/master/src/Data/Links.elm) and add a new record in the list inside the `list` function.

The type of the record is:
```
type alias Attributes =
    { name : String               -- Name of the link, also used as ID
    , url : String                -- Url of the link
    , code : String               -- Url to the source code, if available. Otherwise empty string.
    , picture : String            -- Picture representing the link, if available. Otherwise empty string.
    , description : String        -- Description of the link - Empty string if N/A
    , keywords : List String      -- List of related Keywords - Empty list if N/A
    , authors : List String       -- List of related Authors - Empty list if N/A
    }
```

## How to add a Keyword

Edit the file `src/data/Keywords.elm` [clicking here](https://github.com/lucamug/elm-resources/edit/master/src/Data/Keywords.elm)

* a new constructor for the type `Id`
* a new record in the list inside the `list` function.

The type of the record is:
```
type alias Attributes =
    { id : String      -- The type constructor
    , name : String    -- Name, usually the same as the type constructor
    , picture : String -- Picture that represent the keyword
    }
```

## How to add a Person

Edit the file `src/data/People.elm` [clicking here](https://github.com/lucamug/elm-resources/edit/master/src/Data/People.elm) and add

* a new constructor for the type `Id`
* a new record in the list inside the `list` function.

The type of the record is:
```
type alias Attributes =
    { id : Id          -- The type constructor
    , name : String    -- Name, usually the same as the type constructor
    , picture : String -- Picture that represent the person
    , twitter : String -- Twitter handle
    , github : String  -- Github handle
    , url : String     -- Link to a personal homepage
    }
```

## Development

To run the app

```
npm install
npm start
```

## Production

To build the app

```
cmd/build/start
```
