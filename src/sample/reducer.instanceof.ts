
class UpdateBirthdayAction {
    constructor(public payload: Date){};
}

class UpdateNameAction {
    constructor(public payload: string){};
}

type State = {
  name: string,
  birthday: Date
}

type ActionUnion = UpdateBirthdayAction | UpdateNameAction;

function reducer(state: State, action: ActionUnion): State {
        if (action instanceof UpdateBirthdayAction)
            return { ...state, birthday: action.payload };
        
        if (action instanceof UpdateNameAction)
            return { ...state, name: action.payload };
        
        return state;
}

console.log(reducer({
    name: '',
    birthday: null,
}, new UpdateBirthdayAction(new Date())));