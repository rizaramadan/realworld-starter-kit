namespace Shared

open System

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type UserLoginDto = {
    email:    string
    token:    string
    username: string
    bio:      string
    image:    string
}

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of UserLoginDto

type Users =
    static member login email password =
        async {
            return LoggedIn {
                email = email;
                token =    "token";
                username = "username";
                bio =      "bio";
                image =    "image";
            }
        }

