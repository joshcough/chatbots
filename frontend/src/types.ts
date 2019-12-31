type UserToken = IUserToken;

interface IUserToken {
  userTokenToken: Token;
}

type Token = IToken;

interface IToken {
  getToken: string;
}

type CreateUser = ICreateUser;

interface ICreateUser {
  createUserName: string;
  createUserEmail: string;
  createUserPassword: string;
}

type User = IUser;

interface IUser {
  userId: number;
  userName: string;
  userEmail: string;
  userAdmin: boolean;
}

type Login = ILogin;

interface ILogin {
  loginEmail: string;
  loginPassword: string;
}

type ChannelName = IChannelName;

interface IChannelName {
  _unChannelName: string;
}

type ChatUser = IChatUser;

interface IChatUser {
  cuUserName: ChatUserName;
  cuMod: boolean;
  cuSubscriber: boolean;
}

type ChatUserName = IChatUserName;

interface IChatUserName {
  cunName: string;
}

type Command = ICommand;

interface ICommand {
  commandChannel: ChannelName;
  commandName: string;
  commandBody: string;
}

type Quote = IQuote;

interface IQuote {
  quoteChannel: ChannelName;
  quoteBody: string;
  quoteUser: ChatUserName;
  quoteQid: number;
}

type Stream = IStream;

interface IStream {
  _streamId: number;
  _streamChannelName: ChannelName;
}

/* Example code
function printIStream(iStream: IStream) {
    console.log(iStream._streamId);
    console.log(iStream._streamChannelName);
}

let myObj = {_streamId: 10, _streamChannelName: {_unChannelName : "daut"}};
printIStream(myObj);
*/