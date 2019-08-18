type ActionTypes = 'UpdateBirthday' | 'UpdateName';

type UpdateBirthdayAction = {
    type: ActionTypes;
    payload: Date;
}

type UpdateNameAction = {
    type: ActionTypes;
    payload: string;
}

type State = {
  name: string,
  birthday: Date
}

type ActionUnion = UpdateBirthdayAction | UpdateNameAction;

type ActionBase = { type: ActionTypes }

function reducer(state: State, action: ActionUnion): State {
    switch (action.type) {
        case 'UpdateBirthday': return { ...state, birthday: (action as any).payload };
        case 'UpdateName': return { ...state, name: (action as any).payload };
        default: return state;
    }
}

console.log(reducer({
    name: '', birthday: null
}, {
    type: 'UpdateBirthday',
    payload: new Date()
 }));