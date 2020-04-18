---
title: Optionality and defaulting in ADL
author: Tim Docker
date: 2020-04-17
tags: [programming]
---

[ADL][adl] is a data modeling language, which also specifies a [serialization
schema][serialization]. Values can be be optional in the data model, and independently
defaulted in deserialization and value construction.

In ADL, optionality in the data model is part of a value's type. One uses either the
`Nullable<T>` [primitive][] or the `Maybe<T>` type from the [adl standard library][stdlib]. For
example:

```
struct Person {
  String name;
  Nullable<String> phoneNumber;
};
```

In our model, every person has a name, but a having phone number is optional. But according
to the ADL serialization [specification][serialization], both fields must still be present in the serialized
value. Hence `{"name":"Tim"}` is invalid. If Tim doesn't have a phone, you'd need to serialize
as `{"name":"Tim": "phoneNumber": null}`.

If you want a field to be defaulted in the serialized form, you must provide a default value
in the ADL type, ie:

```
struct Person {
  String name;
  Nullable<String> phoneNumber = null;
};
```

With this type, `{"name":"Tim"}`  would be a valid value. (Note that defaults can be fully
structured values, not just primitives)

This distinction is important,  as it's often useful to have default values that are not
optional. Consider when we need to extend Person with gender information. If we do it
in this way:

```
struct Person {
  String name;
  Nullable<String> phoneNumber = null;
  Gender gender = "unspecified";
};

union Gender {
  Void female;
  Void male;
  Void unspecified;
};
```

then every pre-existing serialized Person value will still be valid, and will assume
a gender value of unspecified.

Another use for defaults without optionality is where we have large data types with
many fields values, most of which are defaulted. As a concrete example, consider
a configuration for an application web server:

```
struct MyAppServerConfig {
  DbConnectionConfig dbConnection;
  
  Word16 httpPort = 8080;
  LogLevel logLevel = "error";
};

struct DbConnectionConfig {
  String host;
  Word16 port = 5432;
  String dbName = "myapp";
  String username;
  String password;
  Word16 connectionPoolMinSize = 4;
  Word16 connectionPoolMaxSize = 16;
};
```

In this case one only needs to provide values for the db host, username and password and
can rely on the defaults for the other fields:

```
{
  "dbConnection" : {
    "host": "localhost",
    "username": "test",
    "password": "test"
  }
}
```

Note that defaults are not only used in deserialization. In the ADL language backends
only the non defaulted fields need to be specified when constructing an in memory ADL
value.
     
## on Maybe\<T> vs Nullable\<T>

As mentioned above, ADL has two parameterized types representing optionality: the
`Nullable<T>` [primitive][] or the `Maybe<T>` type from the [adl standard library][stdlib].

Originally ADL didn't have the Nullable primitive, relying on `Maybe<T>` from the ADL 
standard library, with the expect definition as a and ADL union (ie sum type). A consequence
with `Maybe<T>` defined in ADL that way is that the serialised Json is as it would be for any
other union:  `"nothing"`  or `{"just": t}`. I was fine with this,  but some users strongly
prefer to see `null` or `t` in the json. So the `Nullable<T>` primitive was added, that
serializes in the way that people expect.

Note that `Nullable<T>` is less expressive than `Maybe<T>` in  that you  can't usefully
nest it. `Maybe<Maybe<T>>` is semantically useful, where as `Nullable<Nullable<T>>` is not,
as the serialized representation can't represent all of the types values.

Hence `Nullable<T>` should only be used when `T` does not permit a serialized `null`. (TODO:
make this a type check in the ADL compiler).

[adl]:https://github.com/timbod7/adl
[serialization]: https://github.com/timbod7/adl/blob/master/docs/serialization.md
[primitive]:https://github.com/timbod7/adl/blob/master/docs/language.md#primitive-types
[stdlib]:https://github.com/timbod7/adl/blob/master/adl/stdlib/sys/types.adl
